package gov.nih.mipav.model.algorithms;

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
    //   de ,  step , and  intrp .  ode  calls  de .  de  is a supervisor which
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
    //
    //   first call to ode --
    //
    //   the user must provide storage in his calling program for the arrays
    //   in the call list,
    //      y(neqn)
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
    
    // From work storage in original program:
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
    
    // From iwork storage in original program
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
    private int maxnum = 16000;
    
    private boolean testMode = false;
    
    private int testCase;
    
    private final int ENRIGHT_AND_PRYCE_A1 = 0;
    private final int ENRIGHT_AND_PRYCE_A2 = 1;
    private final int ENRIGHT_AND_PRYCE_A3 = 2;
    private final int ENRIGHT_AND_PRYCE_A4 = 3;
    private final int ENRIGHT_AND_PRYCE_A5 = 4;
    private final int ENRIGHT_AND_PRYCE_B1 = 5;
    private final int ENRIGHT_AND_PRYCE_B2 = 6;
    private final int ENRIGHT_AND_PRYCE_B3 = 7;
    private final int ENRIGHT_AND_PRYCE_B4 = 8;
    private final int ENRIGHT_AND_PRYCE_B5 = 9;
    private final int ENRIGHT_AND_PRYCE_C1 = 10;
    private final int ENRIGHT_AND_PRYCE_C2 = 11;
    private final int ENRIGHT_AND_PRYCE_C3 = 12;
    private final int ENRIGHT_AND_PRYCE_C4 = 13;
    private final int ENRIGHT_AND_PRYCE_C5 = 14;
    private final int ENRIGHT_AND_PRYCE_D1 = 15;
    private final int ENRIGHT_AND_PRYCE_D2 = 16;
    private final int ENRIGHT_AND_PRYCE_D3 = 17;
    private final int ENRIGHT_AND_PRYCE_D4 = 18;
    private final int ENRIGHT_AND_PRYCE_D5 = 19;
    private final int ENRIGHT_AND_PRYCE_E1 = 20;
    private final int ENRIGHT_AND_PRYCE_E2 = 21;
    private final int ENRIGHT_AND_PRYCE_E3 = 22;
    private final int ENRIGHT_AND_PRYCE_E4 = 23;
    private final int ENRIGHT_AND_PRYCE_E5 = 24;
    private final int ENRIGHT_AND_PRYCE_F1 = 25;
    private final int ENRIGHT_AND_PRYCE_F2 = 26;
    private final int ENRIGHT_AND_PRYCE_F3 = 27;
    private final int ENRIGHT_AND_PRYCE_F4 = 28;
    private final int ENRIGHT_AND_PRYCE_F5 = 29;
    private final int LOTKA_VOLTERRA_PREDATOR_PREY = 30;
    private final int LORENZ_SYSTEM = 31;
    private final int VAN_DER_POL = 32;
    private final int LINEARIZED_DAMPED_PENDULUM = 33;
    private final int NONLINEAR_DAMPED_PENDULUM = 34;
    private final int DUFFINGS = 35;
    private final int DUFFINGS_WITH_DAMPING_AND_FORCING = 36;
    private final int SHAMPINES_BALL_OF_FLAME = 37;
    private final int POLKINGS_FIRST_ORDER = 38;
    private final int KNEE_PROBLEM = 39;
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
    	// Passed 34 out of 39 tests
    	// 1.) ENRIGHT_AND_PRYCE_F1 Calculated y[0] correctly, failed on y[1]
    	// Answers for these last 2,3, and 4 are uncertain:
    	// 2.) NONLINEAR_DAMPED_PENDULUM Answer a bit off
    	// 3.) DUFFINGS_WITH_DAMPING_AND_FORCING_FAILED
    	// 4.) POLKINGS_FIRST_ORDER Off 1%
    	// 5.) KNEE_PROBLEM off slightly from zero
    	// The Lorenz system does not have a well defined stopping point
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
    	Preferences.debug("Actual value = 0.2182178902359887"  + 
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
    	Preferences.debug("Actual value = 2.491650271850414E"  + 
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
    	Preferences.debug("Actual value = 17.73016648131483"  + 
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
    	Preferences.debug("Actual value = -0.7887826688964196"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B1;
    	Preferences.debug("Enright and Pryce #B1 neqn = 2 y1' = 2*y1*(1-y2)\n");
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
    	Preferences.debug("Actual value y[0]= 0.6761876008576667"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 0.1860816099640036"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B2;
    	Preferences.debug("Enright and Pryce #B2 neqn = 3 y1' = -y1 + y2\n");
    	Preferences.debug("y2' = y1 - 2*y2 + y3\n");
    	Preferences.debug("y3' = y2 - y3\n");
    	Preferences.debug("y[0] = 2.0  y[1] = 0.0 y[2] = 1.0\n");
    	neqn = 3;
    	y = new double[3];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 0.0;
    	y[2] = 1.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 1.000000001030576"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 1.000000000000000"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= 0.9999999989694235"  + 
				" Calculated value = " + y[2] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B3;
    	Preferences.debug("Enright and Pryce #B3 neqn = 3 y1' = -y1\n");
    	Preferences.debug("y2' = y1 - y2^2\n");
    	Preferences.debug("y3' = y2^2\n");
    	Preferences.debug("y[0] = 2.0  y[1] = 0.0 y[2] = 1.0\n");
    	neqn = 3;
    	y = new double[3];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1.0;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= = 2.061153488557776E-09"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 5.257228022048349E-02"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= 0.9474277177183630"  + 
				" Calculated value = " + y[2] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B4;
    	Preferences.debug("Enright and Pryce #B4 neqn = 3 y1' = (-y2-y1*y3)/sqrt(y1^2+y2^2)\n");
    	Preferences.debug("y2' = (y1 - y2*y3)/sqrt(y1^2+y2^2)\n");
    	Preferences.debug("y3' = y1/sqrt(y1^2+y2^2)n");
    	Preferences.debug("y[0] = 3.0  y[1] = 0.0 y[2] = 0.0\n");
    	neqn = 3;
    	y = new double[3];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 3.0;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 0.9826950928005993"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= = 2.198447081694832"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= 0.9129452507276399"  + 
				" Calculated value = " + y[2] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B5;
    	Preferences.debug("Enright and Pryce #B5 neqn = 3 y1' = y2*y3\n");
    	Preferences.debug("y2' = -y1*y3\n");
    	Preferences.debug("y3' = -0.51*y1*y2\n");
    	Preferences.debug("y[0] = 0.0  y[1] = 1.0 y[2] = 1.0\n");
    	// y1 = sn(x, 0.51)
    	// y2 = cn(x, 0.51)
    	// y3 = dn(x, 0.51)
    	neqn = 3;
    	y = new double[3];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.0;
    	y[1] = 1.0;
    	y[2] = 1.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -0.9396570798729192"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -0.3421177754000779"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= 0.7414126596199957"  + 
				" Calculated value = " + y[2] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C1;
    	Preferences.debug("Enright and Pryce #C1 neqn = 10 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[9] == 0\n");
    	neqn = 10;
    	y = new double[10];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	for (i = 1; i < 10 ; i++) {
    		y[i] = 0;
    	}
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 2.061153622240064D-09"  + 
				" Calculated value = " + y[0] + "\n");
        Preferences.debug("Actual value = 4.122307244619555D-08"  + 
    			" Calculated value = " + y[1] + "\n");
        Preferences.debug("Actual value = 4.122307244716968D-07"  + 
    			" Calculated value = " + y[2] + "\n");
        Preferences.debug("Actual value = 2.748204829855288D-06"  + 
    			" Calculated value = " + y[3] + "\n");
        Preferences.debug("Actual value = 1.374102414941961D-05"  + 
    			" Calculated value = " + y[4] + "\n");
        Preferences.debug("Actual value = 5.496409659803266D-05"  + 
    			" Calculated value = " + y[5] + "\n");
        Preferences.debug("Actual value = 1.832136553274552D-04"  + 
    			" Calculated value = " + y[6] + "\n");
        Preferences.debug("Actual value = 5.234675866508716D-04"  + 
    			" Calculated value = " + y[7] + "\n");
        Preferences.debug("Actual value = 1.308668966628220D-03"  + 
    			" Calculated value = " + y[8] + "\n");
        Preferences.debug("Actual value = 9.979127409508656D-01"  + 
    			" Calculated value = " + y[9] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C2;
    	Preferences.debug("Enright and Pryce #C2 neqn = 10 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[9] == 0\n");
    	neqn = 10;
    	y = new double[10];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	for (i = 1; i < 10 ; i++) {
    		y[i] = 0;
    	}
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage()); 
    	Preferences.debug("Actual value = 2.061153577984930D-09"  + 
				" Calculated value = " + y[0] + "\n");
        Preferences.debug("Actual value = 2.061153573736588D-09"  + 
    			" Calculated value = " + y[1] + "\n");
        Preferences.debug("Actual value = 2.061153569488245D-09"  + 
    			" Calculated value = " + y[2] + "\n");
        Preferences.debug("Actual value = 2.061153565239902D-09"  + 
    			" Calculated value = " + y[3] + "\n");
        Preferences.debug("Actual value = 2.061153560991560D-09"  + 
    			" Calculated value = " + y[4] + "\n");
        Preferences.debug("Actual value = 2.061153556743217D-09"  + 
    			" Calculated value = " + y[5] + "\n");
        Preferences.debug("Actual value =  2.061153552494874D-09"  + 
    			" Calculated value = " + y[6] + "\n");
        Preferences.debug("Actual value = 2.061153548246532D-09"  + 
    			" Calculated value = " + y[7] + "\n");
        Preferences.debug("Actual value = 2.061153543998189D-09"  + 
    			" Calculated value = " + y[8] + "\n");
        Preferences.debug("Actual value = 9.999999814496180D-01"  + 
    			" Calculated value = " + y[9] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");	

		testCase = ENRIGHT_AND_PRYCE_C3;
    	Preferences.debug("Enright and Pryce #C3 neqn = 10 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[9] == 0\n");
    	neqn = 10;
    	y = new double[10];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	for (i = 1; i < 10 ; i++) {
    		y[i] = 0;
    	}
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage()); 
    	Preferences.debug("Actual value = 2.948119211022058D-03"  + 
				" Calculated value = " + y[0] + "\n");
        Preferences.debug("Actual value = 5.635380154844266D-03"  + 
    			" Calculated value = " + y[1] + "\n");
        Preferences.debug("Actual value = 7.829072515926013D-03"  + 
    			" Calculated value = " + y[2] + "\n");
        Preferences.debug("Actual value = 9.348257908594937D-03"  + 
    			" Calculated value = " + y[3] + "\n");
        Preferences.debug("Actual value = 1.007943610301970D-02"  + 
    			" Calculated value = " + y[4] + "\n");
        Preferences.debug("Actual value = 9.982674171429909D-03"  + 
    			" Calculated value = " + y[5] + "\n");
        Preferences.debug("Actual value =  9.088693332766085D-03"  + 
    			" Calculated value = " + y[6] + "\n");
        Preferences.debug("Actual value = 7.489115195185912D-03"  + 
    			" Calculated value = " + y[7] + "\n");
        Preferences.debug("Actual value =  5.322964130953349D-03"  + 
    			" Calculated value = " + y[8] + "\n");
        Preferences.debug("Actual value = 2.762434379029886D-03"  + 
    			" Calculated value = " + y[9] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C4;
    	Preferences.debug("Enright and Pryce #C4 neqn = 51 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[50] == 0\n");
    	neqn = 51;
    	y = new double[51];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1;
    	for (i = 1; i < 51 ; i++) {
    		y[i] = 0;
    	}
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 3.124111453721466D-03"  + 
				" Calculated value = " + y[0] + "\n");
        Preferences.debug("Actual value = 6.015416842150318D-03"  + 
    			" Calculated value = " + y[1] + "\n");
        Preferences.debug("Actual value = 8.470021834842650D-03"  + 
    			" Calculated value = " + y[2] + "\n");
        Preferences.debug("Actual value = 1.033682931733337D-02"  + 
    			" Calculated value = " + y[3] + "\n");
        Preferences.debug("Actual value = 1.153249572873923D-02"  + 
    			" Calculated value = " + y[4] + "\n");
        Preferences.debug("Actual value = 1.204549525737964D-02"  + 
    			" Calculated value = " + y[5] + "\n");
        Preferences.debug("Actual value = 1.192957068015293D-02"  + 
    			" Calculated value = " + y[6] + "\n");
        Preferences.debug("Actual value = 1.128883207111195D-02"  + 
    			" Calculated value = " + y[7] + "\n");
        Preferences.debug("Actual value = 1.025804501391024D-02"  + 
    			" Calculated value = " + y[8] + "\n");
        Preferences.debug("Actual value = 8.982017581934167D-03"  + 
    			" Calculated value = " + y[9] + "\n");
        Preferences.debug("Actual value = 7.597500902492453D-03"  + 
				" Calculated value = " + y[10] + "\n");
        Preferences.debug("Actual value = 6.219920556824985D-03"  + 
    			" Calculated value = " + y[11] + "\n");
        Preferences.debug("Actual value = 4.935916341009131D-03"  + 
    			" Calculated value = " + y[12] + "\n");
        Preferences.debug("Actual value = 3.801432544256119D-03"  + 
    			" Calculated value = " + y[13] + "\n");
        Preferences.debug("Actual value = 2.844213677587894D-03"  + 
    			" Calculated value = " + y[14] + "\n");
        Preferences.debug("Actual value = 2.069123394222672D-03"  + 
    			" Calculated value = " + y[15] + "\n");
        Preferences.debug("Actual value =  1.464687282843915D-03"  + 
    			" Calculated value = " + y[16] + "\n");
        Preferences.debug("Actual value = 1.009545263941126D-03"  + 
    			" Calculated value = " + y[17] + "\n");
        Preferences.debug("Actual value =  6.779354330227017D-04"  + 
    			" Calculated value = " + y[18] + "\n");
        Preferences.debug("Actual value = 4.437815269118510D-04"  + 
    			" Calculated value = " + y[19] + "\n");
        Preferences.debug("Actual value = 2.833264542938954D-04"  + 
				" Calculated value = " + y[20] + "\n");
        Preferences.debug("Actual value = 1.765005798796805D-04"  + 
    			" Calculated value = " + y[21] + "\n");
        Preferences.debug("Actual value = 1.073342592697238D-04"  + 
    			" Calculated value = " + y[22] + "\n");
        Preferences.debug("Actual value = 6.374497601777217D-05"  + 
    			" Calculated value = " + y[23] + "\n");
        Preferences.debug("Actual value = 3.698645309704183D-05"  + 
    			" Calculated value = " + y[24] + "\n");
        Preferences.debug("Actual value = 2.097466832643746D-05"  + 
    			" Calculated value = " + y[25] + "\n");
        Preferences.debug("Actual value = 1.162956710412555D-05"  + 
    			" Calculated value = " + y[26] + "\n");
        Preferences.debug("Actual value = 6.306710405783322D-06"  + 
    			" Calculated value = " + y[27] + "\n");
        Preferences.debug("Actual value = 3.346286430868515D-06"  + 
    			" Calculated value = " + y[28] + "\n");
        Preferences.debug("Actual value = 1.737760074184334D-06"  + 
    			" Calculated value = " + y[29] + "\n");
        Preferences.debug("Actual value = 8.835366904275847D-07"  + 
				" Calculated value = " + y[30] + "\n");
        Preferences.debug("Actual value = 4.399520411127637D-07"  + 
    			" Calculated value = " + y[31] + "\n");
        Preferences.debug("Actual value = 2.146181897152360D-07"  + 
    			" Calculated value = " + y[32] + "\n");
        Preferences.debug("Actual value = 1.025981211654928D-07"  + 
    			" Calculated value = " + y[33] + "\n");
        Preferences.debug("Actual value = 4.807864068784215D-08"  + 
    			" Calculated value = " + y[34] + "\n");
        Preferences.debug("Actual value = 2.209175152474847D-08"  + 
    			" Calculated value = " + y[35] + "\n");
        Preferences.debug("Actual value = 9.956251263138180D-09"  + 
    			" Calculated value = " + y[36] + "\n");
        Preferences.debug("Actual value = 4.402193653748924D-09"  + 
    			" Calculated value = " + y[37] + "\n");
        Preferences.debug("Actual value = 1.910149382204028D-09"  + 
    			" Calculated value = " + y[38] + "\n");
        Preferences.debug("Actual value = 8.135892921473050D-10"  + 
    			" Calculated value = " + y[39] + "\n");
        Preferences.debug("Actual value = 3.402477118549235D-10"  + 
				" Calculated value = " + y[40] + "\n");
        Preferences.debug("Actual value = 1.397485617545782D-10"  + 
    			" Calculated value = " + y[41] + "\n");
        Preferences.debug("Actual value = 5.638575303049199D-11"  + 
    			" Calculated value = " + y[42] + "\n");
        Preferences.debug("Actual value = 2.235459707956947D-11"  + 
    			" Calculated value = " + y[43] + "\n");
        Preferences.debug("Actual value = 8.710498036398032D-12"  + 
    			" Calculated value = " + y[44] + "\n");
        Preferences.debug("Actual value = 3.336554275346643D-12"  + 
    			" Calculated value = " + y[45] + "\n");
        Preferences.debug("Actual value =  1.256679567784939D-12"  + 
    			" Calculated value = " + y[46] + "\n");
        Preferences.debug("Actual value = 4.654359053128788D-13"  + 
    			" Calculated value = " + y[47] + "\n");
        Preferences.debug("Actual value = 1.693559145599857D-13"  + 
    			" Calculated value = " + y[48] + "\n");
        Preferences.debug("Actual value = 5.996593816663054D-14"  + 
    			" Calculated value = " + y[49] + "\n");
        Preferences.debug("Actual value = 1.891330702629865D-14"  + 
				" Calculated value = " + y[50] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C5;
    	Preferences.debug("Enright and Pryce #C5 neqn = 30 \n");
    	neqn = 30;
    	y = new double[30];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 3.42947415189;
    	y[1] = 3.35386959711;
    	y[2] = 1.35494901715;
    	y[3] = 6.64145542550;
    	y[4] = 5.97156957878;
    	y[5] = 2.18231499728;
    	y[6] = 11.2630437207;
    	y[7] = 14.6952576794;
    	y[8] = 6.27960525067;
    	y[9] = -30.1552268759;
    	y[10] = 1.65699966404;
    	y[11] = 1.43785752721;
    	y[12] = -21.1238353380;
    	y[13] = 28.4465098142;
    	y[14] = 15.3882659679;
    	y[15] = -.557160570446;
    	y[16] = .505696783289;
    	y[17] = .230578543901;
    	y[18] = -.415570776342;
    	y[19] = .365682722812;
    	y[20] = .169143213293;
    	y[21] = -.325325669158;
    	y[22] = .189706021964;
    	y[23] = .0877265322780;
    	y[24] = -.0240476254170;
    	y[25] = -.287659532608;
    	y[26] = -.117219543175;
    	y[27] = -.176860753121;
    	y[28] = -.216393453025;
    	y[29] = -.0148647893090;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 3.0E-15;
    	abserr[0] = 3.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -4.792730224323733"  + 
				" Calculated value = " + y[0] + "\n");
        Preferences.debug("Actual value = -2.420550725448973"  + 
    			" Calculated value = " + y[1] + "\n");
        Preferences.debug("Actual value = -9.212509306014886D-01"  + 
    			" Calculated value = " + y[2] + "\n");
        Preferences.debug("Actual value = -4.217310404035213"  + 
    			" Calculated value = " + y[3] + "\n");
        Preferences.debug("Actual value = 7.356202947498970"  + 
    			" Calculated value = " + y[4] + "\n");
        Preferences.debug("Actual value = 3.223785985421212"  + 
    			" Calculated value = " + y[5] + "\n");
        Preferences.debug("Actual value = 4.035559443262270"  + 
    			" Calculated value = " + y[6] + "\n");
        Preferences.debug("Actual value =  1.719865528670555D+01"  + 
    			" Calculated value = " + y[7] + "\n");
        Preferences.debug("Actual value = 7.478910794233703"  + 
    			" Calculated value = " + y[8] + "\n");
        Preferences.debug("Actual value =-2.998759326324844D+01 "  + 
    			" Calculated value = " + y[9] + "\n");
        Preferences.debug("Actual value = -4.107310937550929"  + 
				" Calculated value = " + y[10] + "\n");
        Preferences.debug("Actual value = -9.277008321754407D-01"  + 
    			" Calculated value = " + y[11] + "\n");
        Preferences.debug("Actual value = -2.442125302518482D+01"  + 
    			" Calculated value = " + y[12] + "\n");
        Preferences.debug("Actual value = 2.381459045746554D+01"  + 
    			" Calculated value = " + y[13] + "\n");
        Preferences.debug("Actual value = 1.492096306951359D+01"  + 
    			" Calculated value = " + y[14] + "\n");
        Preferences.debug("Actual value = 3.499208963063806D-01"  + 
    			" Calculated value = " + y[15] + "\n");
        Preferences.debug("Actual value = -5.748487687912825D-01"  + 
    			" Calculated value = " + y[16] + "\n");
        Preferences.debug("Actual value = -2.551694020879149D-01"  + 
    			" Calculated value = " + y[17] + "\n");
        Preferences.debug("Actual value = -5.237040978903326D-01"  + 
    			" Calculated value = " + y[18] + "\n");
        Preferences.debug("Actual value = -2.493000463579661D-01"  + 
    			" Calculated value = " + y[19] + "\n");
        Preferences.debug("Actual value = -8.045341642044464D-02"  + 
				" Calculated value = " + y[20] + "\n");
        Preferences.debug("Actual value = -3.875289237334110D-01"  + 
    			" Calculated value = " + y[21] + "\n");
        Preferences.debug("Actual value = 5.648603288767891D-02"  + 
    			" Calculated value = " + y[22] + "\n");
        Preferences.debug("Actual value = 3.023606472143342D-02"  + 
    			" Calculated value = " + y[23] + "\n");
        Preferences.debug("Actual value =  4.133856546712445D-02"  + 
    			" Calculated value = " + y[24] + "\n");
        Preferences.debug("Actual value = -2.862393029841379D-01"  + 
    			" Calculated value = " + y[25] + "\n");
        Preferences.debug("Actual value = -1.183032405136207D-01"  + 
    			" Calculated value = " + y[26] + "\n");
        Preferences.debug("Actual value = -1.511986457359206D-01"  + 
    			" Calculated value = " + y[27] + "\n");
        Preferences.debug("Actual value = -2.460068894318766D-01"  + 
    			" Calculated value = " + y[28] + "\n");
        Preferences.debug("Actual value = -3.189687411323877D-02"  + 
    			" Calculated value = " + y[29] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D1;
    	Preferences.debug("Enright and Pryce #D1 neqn = 4 \n");
    	neqn = 4;
    	y = new double[4];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	int ID = 31;
    	double E = .2*((double)ID-30.0) - .1;
    	y[0] = 1.0 - E;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	y[3] = Math.sqrt((1.0 + E)/(1.0 - E));
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 2.198835352008397D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 9.427076846341813D-01"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= -9.787659841058176D-01"  + 
				" Calculated value = " + y[2] + "\n");
    	Preferences.debug("Actual value y[3]= 3.287977990962036D-01"  + 
				" Calculated value = " + y[3] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D2;
    	Preferences.debug("Enright and Pryce #D2 neqn = 4 \n");
    	neqn = 4;
    	y = new double[4];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 32;
    	E = .2*((double)ID-30.0) - .1;
    	y[0] = 1.0 - E;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	y[3] = Math.sqrt((1.0 + E)/(1.0 - E));
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -1.777027357140412D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 9.467784719905892D-01"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= -1.030294163192969"  + 
				" Calculated value = " + y[2] + "\n");
    	Preferences.debug("Actual value y[3]= 1.211074890053952D-01"  + 
				" Calculated value = " + y[3] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D3;
    	Preferences.debug("Enright and Pryce #D3 neqn = 4 \n");
    	neqn = 4;
    	y = new double[4];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 33;
    	E = .2*((double)ID-30.0) - .1;
    	y[0] = 1.0 - E;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	y[3] = Math.sqrt((1.0 + E)/(1.0 - E));
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -5.780432953035361D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 8.633840009194193D-01"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= -9.595083730380727D-01"  + 
				" Calculated value = " + y[2] + "\n");
    	Preferences.debug("Actual value y[3]= -6.504915126712089D-02"  + 
				" Calculated value = " + y[3] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D4;
    	Preferences.debug("Enright and Pryce #D4 neqn = 4 \n");
    	neqn = 4;
    	y = new double[4];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 34;
    	E = .2*((double)ID-30.0) - .1;
    	y[0] = 1.0 - E;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	y[3] = Math.sqrt((1.0 + E)/(1.0 - E));
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -9.538990293416394D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 6.907409024219432D-01"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= -8.212674270877433D-01"  + 
				" Calculated value = " + y[2] + "\n");
    	Preferences.debug("Actual value y[3]= -1.539574259125825D-01"  + 
				" Calculated value = " + y[3] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D5;
    	Preferences.debug("Enright and Pryce #D5 neqn = 4 \n");
    	neqn = 4;
    	y = new double[4];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 35;
    	E = .2*((double)ID-30.0) - .1;
    	y[0] = 1.0 - E;
    	y[1] = 0.0;
    	y[2] = 0.0;
    	y[3] = Math.sqrt((1.0 + E)/(1.0 - E));
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -1.295266250987574"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 4.003938963792321D-01"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value y[2]= -6.775390924707566D-01"  + 
				" Calculated value = " + y[2] + "\n");
    	Preferences.debug("Actual value y[3]= -1.270838154278686D-01"  + 
				" Calculated value = " + y[3] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E1;
    	Preferences.debug("Enright and Pryce #E1 neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	E = .79788456080286536;
    	y[0] = E*.84147098480789651;
    	y[1] = E*.11956681346419146;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 1.456723600728308D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -9.883500195574063D-02"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E2;
    	Preferences.debug("Enright and Pryce #E2 neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]=  2.008149762174948"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -4.250887527320057D-02"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");

		testCase = ENRIGHT_AND_PRYCE_E3;
    	Preferences.debug("Enright and Pryce #E3 neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.0;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -1.004178858647128D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 2.411400132095954D-01"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E4;
    	Preferences.debug("Enright and Pryce #E4 neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 30.0;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 3.395091444646555D+01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 2.767822659672869D-01"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E5;
    	Preferences.debug("Enright and Pryce #E5 neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.0;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.1E-15;
    	abserr[0] = 1.1E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 1.411797390542629D+01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 2.400000000000002"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F1;
    	Preferences.debug("Enright and Pryce #F1 neqn = 2 \n");
    	// Calculated y[0] correctly, failed on y[1}:
    	// In ODE normal return.  Integration reached tout
        // Actual value y[0]= -1.294460621213470D1 Calculated value = -12.944606212133303
        // Actual value y[1]= -2.208575158908672D-15 Calculated value = 1.0635633256705237E-11
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.0;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 4.0E-14;
    	abserr[0] = 4.0E-14;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -1.294460621213470D1"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -2.208575158908672D-15"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F2;
		Preferences.debug("Enright and Pryce #F2 neqn = 1 \n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 110.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-14;
    	abserr[0] = 1.0E-14;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 70.03731057008607"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F3;
		Preferences.debug("Enright and Pryce #F3 neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.0;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -3.726957553088175D-1"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = -6.230137949234190D-1"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F4;
		Preferences.debug("Enright and Pryce #F4 neqn = 1 \n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 9.815017249707434D-11"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F5;
		Preferences.debug("Enright and Pryce #F5 neqn = 1 \n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 1.0"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = LOTKA_VOLTERRA_PREDATOR_PREY;
		Preferences.debug("Lotka-Volterra Predator-Prey Equations neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 2.0;
    	t[0] = 0;
    	tout = 10.0;
    	relerr[0] = 2.0E-15;
    	abserr[0] = 2.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 2.20050"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 10.2726"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = LORENZ_SYSTEM;
		Preferences.debug("The Lorenz System neqn = 3 \n");
		// The discussion for P32_STOP says:
		// The system is chaotic, and so a dummy stop value is put here.
		// so essentially no stop value is given
    	neqn = 3;
    	y = new double[3];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 2.0;
    	y[2] = 21.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 2.0E-15;
    	abserr[0] = 2.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = ?"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = ?"  + 
				" Calculated value = " + y[1] + "\n");
    	Preferences.debug("Actual value = ?"  + 
				" Calculated value = " + y[2] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = VAN_DER_POL;
		Preferences.debug("The Van der Pol equation neqn = 2 \n");
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 2.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 0.756245"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 2.67294"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = LINEARIZED_DAMPED_PENDULUM;
		Preferences.debug("The Linearized Damped Pendulum neqn = 2 \n");
		// yy[0] = x
		// yy[1] = x'
		// yp[0] = x'
		// yp[1] = x"
		// x" + (g/length)x + (d/m)x' = 0
		// x" + 32x + x' = 0
		// Let x = exp(rt)
		// r^2 + r + 32 = 0
		// r = -0.5 +-jsqrt(127)/2
		// x = Aexp(-t/2)cos(sqrt(127)*t/2) + Bexp(-t/2)sin(sqrt(127)*t/2)
		// At t = 0, yy[0] = A = 2
		// x' = exp(-t/2)[(-1 + B*sqrt(127)/2)*cos(sqrt(127)*t/2) +
		// (-B/2 - sqrt(127)*sin(sqrt(127)*t/2)]
		// At t = 0, yy[1] = 2 = B*sqrt(127)/2 - 1
		// B = 6/sqrt(127)
		// x = 2exp(-t/2)cos(sqrt(127)*t/2) + (6/sqrt(127))exp(-t/2)sin(sqrt(127)*t/2)
		//double sqrt127 = Math.sqrt(127.0);
		//double yout0 = Math.exp(-10.0)*(2.0*Math.cos(10.0*sqrt127) - (6/sqrt127)*Math.sin(10.0*sqrt127));
		// gives yout0 = 7.404275751842365E-5
		//double yout1 =  Math.exp(-10.0)*(2.0*Math.cos(10.0*sqrt127) + (-3.0/sqrt127 - sqrt127)*Math.sin(10.0*sqrt127));
		// gives yout1 = 2.8894321598544795E-4
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 2.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 7.404275751842365E-5"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 2.8894321598544795E-4"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = NONLINEAR_DAMPED_PENDULUM;
		Preferences.debug("The Nonlinear Damped Pendulum neqn = 2 \n");
		// A bit off:
		// In ODE normal return.  Integration reached tout
		// Actual value = -5.84253E-5 Calculated value = -6.123467982092676E-5
	    // Actual value = 3.59969E-4 Calculated value = 3.8103739806513953E-4
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 2.0;
    	y[1] = 2.0;
    	t[0] = 0;
    	tout = 20.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -5.84253E-5"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 3.59969E-4"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = DUFFINGS;
		Preferences.debug("Duffing's Equation neqn = 2 \n");
		// From Exact Solution to the Duffing Equation and the Pendulum Equation by
		// Alvaro H. Salas
		// For x" + alpha*x + beta*x^3 = 0 for x(0) = x0 and x'(0) = 0
		// With alpha = -1, beta = 1, x0 = 0.5, x'(0) = 0
		// x(t) = x0*cn(sqrt(alpha + beta*x0^2)*tout, sqrt((beta*x0^2)/(2*(alpha + beta*x0^2)))
		// for (alpha + beta*x0^2) != 0 
		// x(t) = 0.5*cn(sqrt(-3/4)*t, sqrt(-1/6))
		// From Exact solutions to cubic duffing equation for a nonlinear electrical circuit
		// by Alvaro H. Salas and Jairo E. Castillo
		// For both arguments pure imaginary:
		// cn(iwt, im) = 1/dn(sqrt(m^2 + 1)*w*t,1/sqrt(m^2 + 1)) 
		// d(dn(t,m))/dt = -m^2*sn(t,m)*cn(t,m)
		// Derivative = 0.5*m^2*(m^2 + 1)*w*sn*cn/(dn*dn)
		//double esn[] = new double[1];
		//double ecn[] = new double[1];
		//double edn[] = new double[1];
		//double esd[] = new double[1];
		//double dph[] = new double[1];
		//JacobianElliptic je = new JacobianElliptic(Math.sqrt(7.0/8.0)*100.0, Math.sqrt(6.0/7.0), esn, ecn, edn,
		//		esd, dph);
		//je.run();
		//double calculatedy0 = 0.5/edn[0];
		//System.out.println("calculatedy0 = " + calculatedy0);
		//double calculatedy1 = 0.5*(6.0/7.0)*Math.sqrt(7.0/8.0)*esn[0]*ecn[0]/(edn[0]*edn[0]);
		//System.out.println("calculatedy1 = " + calculatedy1);
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.5;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 100.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 1.18955786581925"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 0.4417211944847741"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = DUFFINGS_WITH_DAMPING_AND_FORCING;
		Preferences.debug("Duffing's Equation with Damping and Forcing neqn = 2 \n");
		tout = 100.0;
		// From Approximate Solution of Nonlinear Duffing Oscillator Using Taylor
		// Expansion by A. Okasha El-Nady and Maha M.A.Lashin
    	// double delt = 1.0E-6;
    	// 0 to tout + delt
    	// int timePositions = (int)Math.round(tout/delt + 2);
    	// double x[] = new double[timePositions];
    	// double dx[] = new double[timePositions];
    	// double dx2[] = new double[timePositions];
    	// x[0] = 0.5;
    	// dx[0] = 0.0;
    	// dx2[0] = x[0] - x[0]*x[0]*x[0] - 0.2*dx[0] + 0.3*Math.cos(0.0);
    	// x[1] = x[0] + dx[0]*delt + 0.5*dx2[0]*delt*delt;
    	// x[2] = x[0] + 2.0*dx[0]*delt + 2.0*dx2[0]*delt*delt;
    	// dx[1] = (x[2] - x[0])/(2.0*delt);
    	// // dx2[1] = x[1] - x[1]*x[1]*x[1] - 0.2*dx[1] + 0.3*Math.cos(delt);
    	// for (i = 2; i < timePositions-1; i++) {
    		// x[i] = x[i-1] + dx[i-1]*delt + 0.5*dx2[i-1]*delt*delt;
    		// x[i+1] = x[i-1] + 2.0*dx[i-1]*delt + 2.0*dx2[i-1]*delt*delt;
    		// dx[i] = (x[i+1] - x[i-1])/(2.0*delt);
    		// dx2[i] = x[i] - x[i]*x[i]*x[i] - 0.2*dx[i] + 0.3*Math.cos((i-1)*delt);
    	// } 
    	// double yout0 = x[timePositions - 2];
    	// System.out.println("yout0 = " + yout0);
    	// double yout1 = dx[timePositions - 2];
    	// System.out.println("yout1 = " + yout1);\
		// DoubleDouble recurrence equations at delt = 1.0E-6 yield
    	// yout0 = -0.90072788840276037178110317710702
        // yout1 = -0.59945614403229628906295032931339
		// Failed with:
		// In ODE normal return.  Integration reached tout
	    // Actual value = -1.21774 Calculated value = -0.6103319657339887
		// Actual value = -0.548248 Calculated value = 0.21114867070040705
    	neqn = 2;
    	y = new double[2];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.5;
    	y[1] = 0.0;
    	t[0] = 0;
    	tout = 100.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -1.21774"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = -0.548248"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = SHAMPINES_BALL_OF_FLAME;
    	Preferences.debug("Shampine's Ball of Flame neqn = 1 y' = y^2-y^3\n");
    	Preferences.debug("y[0] = 1.0E-2\n");
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 1.0E-2;
    	t[0] = 0;
    	tout = 200.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 1.00000"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = POLKINGS_FIRST_ORDER;
    	Preferences.debug("Polking's first order ODE neqn = 1 \n");
    	// Off slightly:
    	// In ODE normal return.  Integration reached tout
        // Actual value = -3.00000 Calculated value = -2.9715400444864772
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = 0.5;
    	t[0] = 0;
    	tout = 9.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -3.00000"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
		
		testCase = KNEE_PROBLEM;
    	Preferences.debug("the Knee problem neqn = 1 \n");
    	// Using the below equation gives yout = -0.0
    	// DoubleDouble version also gives yout = -0.0
    	//double e = 0.01;
    	//double yout = - 2.0 * Math.sqrt ( e )
    	//	    * Math.exp ( ( 1.0 - tout * tout ) / ( 2.0 * e ) ) / 
    	//	    ( 
    	//	      2 * Math.sqrt ( e ) + 
    	//	      ( 
    	//	        errorFunction ( 1.0 / Math.sqrt ( 2.0 * e ) )  + 
    	//	        errorFunction ( tout  / Math.sqrt ( 2.0 + e ) ) 
    	//	      ) 
    	//	      * Math.exp ( 0.5 / e ) * Math.sqrt ( 2.0 * Math.PI ) 
    	//	    );
    	// Failed with:
    	// In ODE normal return.  Integration reached tout
    	// Actual value = -0.0 Calculated value = 1.2249382520735495E-15
    	neqn = 1;
    	y = new double[1];
    	t = new double[1];
    	relerr = new double[1];
    	abserr = new double[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = -1.0;
    	t[0] = -1.0;
    	tout = 1.0;
    	relerr[0] = 1.0E-15;
    	abserr[0] = 1.0E-15;
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -0.0"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t[0] + "\n");
		Preferences.debug("relerr = " + relerr[0] + " abserr = " + abserr[0] + "\n");
    	
    }
    
    //private double errorFunction(double x) { 
    	// From Computation of Special Functions by Shanjie Zhang and Jianming Jin,
    	// pp.620-623.
    	//double er;
    	//double err;
    	//double r;
    	//int k;
    	//double c0;
    	//double eps = 1.0E-15;
    	//double x2 = x * x;
    	//double sqrtPi = Math.sqrt(Math.PI);
    	//if (Math.abs(x) < 3.5) {
    	    //er = 1.0;
    	    //r = 1.0;
    	    //for (k = 1; k <= 50; k++) {
    	        //r = r*x2/(k+0.50);
    	        //er = er + r;
    	        //if (Math.abs(r) <= Math.abs(er)*eps) {
    	        	//break;
    	       // }
    	    //} // for (k = 1; k <= 50; k++)
    	    //c0 = 2.0/sqrtPi*x*Math.exp(-x2);
    	    //err = c0*er;
    	//} // if (Math.abs(x) < 3.5)
    	//else {
    		//er = 1.0;
    		//r = 1.0;
    		//for (k = 1; k <= 12; k++) {
    			//r = -r*(k-0.5)/x2;
    			//er = er + r;
    		//}
    		//c0 = Math.exp(-x2)/(Math.abs(x)*sqrtPi);
    		//err = 1.0 - c0*er;
    		//if (x < 0.0) {
    			//err = -err;
    		//}
    	//} // else
    	//return err;
    //}
    
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
    	int i;
    	int i3;
    	int i3m2;
    	int itemp;
    	int j;
    	int l;
    	int ll;
    	int mm;
    	double a;
    	double b;
    	double c;
    	double d;
    	double denom;
    	double diff1;
    	double diff2;
    	double diff3;
    	double p;
    	double Q[][];
    	double r[];
    	double temp;
    	double var;
    	double g = 32.0;
    	double length = 1.0;
    	double dampingCoefficient = 1.0;
    	double mass = 1.0;
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
        case ENRIGHT_AND_PRYCE_B2:
        	yp[0] = -yy[0] + yy[1];
        	yp[1] = yy[0] - 2*yy[1] + yy[2];
        	yp[2] = yy[1] - yy[2];
        	break;
        case ENRIGHT_AND_PRYCE_B3:
        	yp[0] = -yy[0];
        	yp[1] = yy[0] - yy[1]*yy[1];
        	yp[2] = yy[1]*yy[1];
        	break;
        case ENRIGHT_AND_PRYCE_B4:
            denom = Math.sqrt(yy[0]*yy[0] + yy[1]*yy[1]);
            yp[0] = -yy[1] - yy[0]*yy[2]/denom;
            yp[1] = yy[0] - yy[1]*yy[2]/denom;
            yp[2] = yy[0]/denom;
            break;
        case ENRIGHT_AND_PRYCE_B5:
        	yp[0] = yy[1]*yy[2];
        	yp[1] = -yy[0]*yy[2];
        	yp[2] = -0.51*yy[0]*yy[1];
        	break;
        case ENRIGHT_AND_PRYCE_C1:
            yp[0] = -yy[0];
        	for (i = 1; i <= 8; i++) {
        		yp[i] = yy[i-1] - yy[i];
        	}
        	yp[9] = yy[8];
        	break;
        case ENRIGHT_AND_PRYCE_C2:
            yp[0] = -yy[0];
        	for (i = 2; i <= 9; i++) {
        		yp[i-1] = (i-1)*yy[i-2] - i*yy[i-1];
        	}
        	yp[9] = 9.0*yy[8];
        	break;
        case ENRIGHT_AND_PRYCE_C3:
            yp[0] = -2.0*yy[0] + yy[1];
        	for (i = 1; i <= 8; i++) {
        		yp[i] = yy[i-1] - 2.0*yy[i] + yy[i+1];
        	}
        	yp[9] = yy[8] - 2.0*yy[9];
            break;
        case ENRIGHT_AND_PRYCE_C4:
            yp[0] = -2.0*yy[0] + yy[1];
        	for (i = 1; i <= 49; i++) {
        		yp[i] = yy[i-1] - 2.0*yy[i] + yy[i+1];
        	}
        	yp[50] = yy[49] - 2.0*yy[50];
            break;
        case ENRIGHT_AND_PRYCE_C5:
        	r = new double[5];
        	Q = new double[5][5];
        	// THE FOLLOWING DATA IS FOR PROBLEM C5 AND DEFINES THE MASSES
        	// OF THE 5 OUTER PLANETS ETC. IN SOLAR UNITS.
        	// K2 IS THE GRAVITATIONAL CONSTANT.

        	double m0 = 1.00000597682;
        	double m[] = new double[]{.954786104043E-3,
                             .285583733151E-3, .437273164546E-4,
                            .517759138449E-4, .277777777778E-5};
        	double k2 = 2.95912208286;
        	i = 0;
            for (l = 3; l <= 15; l +=3) {
               i = i + 1;
               p = yy[l-3]*yy[l-3] + yy[l-2]*yy[l-2] + yy[l-1]*yy[l-1];
               r[i-1] = 1.0/(p*Math.sqrt(p));
               j = 0;
               for (ll = 3; ll <= 15; ll += 3) {
                  j = j + 1;
                  if (ll == l) {
                	  continue;
                  }
                  diff1 = yy[l-3] - yy[ll-3];
                  diff2 = yy[l-2] - yy[ll-2];
                  diff3 = yy[l-1] - yy[ll-1];
                  p = diff1*diff1 + diff2*diff2 + diff3*diff3;
                  Q[i-1][j-1] = 1.D/(p*Math.sqrt(p));
                  Q[j-1][i-1] = Q[i-1][j-1];
               } // for (ll = 3; ll <= 15; ll += 3)
            } // for (l = 3; l <= 15; l +=3)
            i3 = 0;
            for (i = 1; i <= 5; i++) {
               i3 = i3 + 3;
               i3m2 = i3 - 2;
               for (ll = i3m2; ll <= i3; ll++) {
                  mm = ll - i3;
                  yp[ll-1] = yy[ll+14];
                  p = 0.0;
                  for (j = 1; j <= 5; j++) {
                     mm = mm + 3;
                     if (j != i) { 
                    	 p = p + m[j-1]*(yy[mm-1]*(Q[i-1][j-1]-r[j-1])-yy[ll-1]*Q[i-1][j-1]);
                     } // if (j != i)
                  } // for (j = 1; j <= 5; j++)
                  yp[ll+14] = k2*(-(m0+m[i-1])*yy[ll-1]*r[i-1]+p);
               } // for (ll = i3m2; ll <= i3; ll++)
            } // for (i = 1; i <= 5; i++)

        	break;
        case ENRIGHT_AND_PRYCE_D1:
        case ENRIGHT_AND_PRYCE_D2:
        case ENRIGHT_AND_PRYCE_D3:
        case ENRIGHT_AND_PRYCE_D4:
        case ENRIGHT_AND_PRYCE_D5:
            yp[0] = yy[2];
        	yp[1] = yy[3];
        	denom = yy[0]*yy[0] + yy[1]*yy[1];
        	denom = Math.sqrt(denom*denom*denom);
        	yp[2] = -yy[0]/denom;
        	yp[3] = -yy[1]/denom;
        	break;
        case ENRIGHT_AND_PRYCE_E1:
            var = x + 1.0;
            yp[0] = yy[1];
        	yp[1] = -(yy[1]/var+(1.0 -0.25/(var*var))*yy[0]);
        	break;
        case ENRIGHT_AND_PRYCE_E2:
            yp[0] = yy[1];
        	yp[1] = (1.0 - yy[0]*yy[0])*yy[1] - yy[0];
            break;
        case ENRIGHT_AND_PRYCE_E3:
            yp[0] = yy[1];
        	yp[1] = yy[0]*yy[0]*yy[0]/6.0 - yy[0] + 2.0*Math.sin(2.78535 * x);
            break;
        case ENRIGHT_AND_PRYCE_E4
:           yp[0] = yy[1];
            yp[1] = 0.032 - 0.4*yy[1]*yy[1];
            break;
        case ENRIGHT_AND_PRYCE_E5:
            yp[0] = yy[1];
        	yp[1] = Math.sqrt(1.0 + yy[1]*yy[1])/(25.0-x);
        	break;
        case ENRIGHT_AND_PRYCE_F1:
        	// C1 IS PI**2 + 0.1**2 
        	double c1 = Math.PI*Math.PI + 0.1*0.1;
            yp[0] = yy[1];
        	yp[1] = 0.2*yy[1] - c1*yy[0];
        	itemp = (int)Math.floor(x);
        	if ((itemp/2)*2 != itemp) {
        		yp[1] = yp[1] - 1.0;
        	}
        	else {
        		yp[1] = yp[1] + 1.0;
        	}
        	break;
        case ENRIGHT_AND_PRYCE_F2:
            itemp = (int)Math.floor(x);
        	if ((itemp/2)*2 != itemp) {
        		yp[0] = 55.0 - 0.5*yy[0];
        	}
        	else {
        		yp[0] = 55.0 - 1.5*yy[0];
        	}
        	break;
        case ENRIGHT_AND_PRYCE_F3:
            yp[0] = yy[1];
        	yp[1] = 0.01*yy[1]*(1.0 - yy[0]*yy[0]) - yy[0] - Math.abs(Math.sin(Math.PI*x));
        	break;
        case ENRIGHT_AND_PRYCE_F4:
            if (x <= 10.0) {
                temp = x - 5.0;	
                yp[0] = -2.0/21.0 - 120.0*temp/Math.pow((1.0 + 4.0*temp*temp), 16);
            }
            else {
            	yp[0] = -2.0*yy[0];
            }
        	break;
        case ENRIGHT_AND_PRYCE_F5:
        	double ex = 1.0/3.0;
        	// C2 IS SUM I**(4/3) FOR I=1 TO 19.
        	double c2 = 0.0;
        	for (i = 1; i <= 19; i++) {
        		c2 += Math.pow(i, (4.0/3.0));
        	}
        	yp[0] = yy[0]*(4.0/(3.0*c2))*(dsign(Math.pow(Math.abs(x-1.0),ex),x-1.0)
             +dsign(Math.pow(Math.abs(x-2.0),ex),x-2.0)
             +dsign(Math.pow(Math.abs(x-3.0),ex),x-3.0)
             +dsign(Math.pow(Math.abs(x-4.0),ex),x-4.0)
             +dsign(Math.pow(Math.abs(x-5.0),ex),x-5.0)
             +dsign(Math.pow(Math.abs(x-6.0),ex),x-6.0)
             +dsign(Math.pow(Math.abs(x-7.0),ex),x-7.0)
             +dsign(Math.pow(Math.abs(x-8.0),ex),x-8.0)
             +dsign(Math.pow(Math.abs(x-9.0),ex),x-9.0)
             +dsign(Math.pow(Math.abs(x-10.0),ex),x-10.0)
             +dsign(Math.pow(Math.abs(x-11.0),ex),x-11.0)
             +dsign(Math.pow(Math.abs(x-12.0),ex),x-12.0)
             +dsign(Math.pow(Math.abs(x-13.0),ex),x-13.0)
             +dsign(Math.pow(Math.abs(x-14.0),ex),x-14.0)
             +dsign(Math.pow(Math.abs(x-15.0),ex),x-15.0)
             +dsign(Math.pow(Math.abs(x-16.0),ex),x-16.0)
             +dsign(Math.pow(Math.abs(x-17.0),ex),x-17.0)
             +dsign(Math.pow(Math.abs(x-18.0),ex),x-18.0)
             +dsign(Math.pow(Math.abs(x-19.0),ex),x-19.0));
        	break;
        case LOTKA_VOLTERRA_PREDATOR_PREY:
        	a = 5.0;
        	b = 1.0;
        	c = 0.5;
        	d = 2.0;
        	yp[0] = (a - b * yy[1]) * yy[0];
        	yp[1] = (c * yy[0] - d) * yy[1];
        	break;
        case LORENZ_SYSTEM:
        	double sigma = 10.0;
        	double rho = 28.0;
        	double beta = 8.0/3.0;
            yp[0] = sigma * (yy[1] - yy[0]);
        	yp[1] = rho * yy[0] - yy[1] - yy[0] * yy[2];
        	yp[2] = -beta * yy[2] + yy[0] * yy[1];
        	break;
        case VAN_DER_POL:
        	double delta = 1.0;
        	yp[0] = yy[1];
        	yp[1] = delta * (1.0 - yy[0]*yy[0]) * yy[1] - yy[0];
        	break;
        case LINEARIZED_DAMPED_PENDULUM:
            yp[0] = yy[1];
        	yp[1] = -(g / length) * yy[0] - (dampingCoefficient / mass) * yy[1];
        	break;
        case NONLINEAR_DAMPED_PENDULUM:
        	yp[0] = yy[1];
        	yp[1] = -(g / length) * Math.sin(yy[0]) - (dampingCoefficient / mass) * yy[1];
        	break;
        case DUFFINGS:
        	yp[0] = yy[1];
        	yp[1] = yy[0] * (1.0 - yy[0]*yy[0]);
        	break;
        case DUFFINGS_WITH_DAMPING_AND_FORCING:
        	double amplitude = 0.3;
        	double kdamp = 0.2;
        	double w = 1.0;
        	yp[0] = yy[1];
        	yp[1] = yy[0] * (1.0 - yy[0]*yy[0]) - kdamp * yy[1] + amplitude * Math.cos(w * x);
        	break;
        case SHAMPINES_BALL_OF_FLAME:
        	double yySquared;
        	yySquared = yy[0]*yy[0];
        	yp[0] = yySquared - yySquared*yy[0];
        	break;
        case POLKINGS_FIRST_ORDER:
        	a = 1.0;
        	b = 0.0;
        	yp[0] = yy[0]*yy[0] - a * x + b;
        	break;
        case KNEE_PROBLEM:
        	double e = 0.01;
        	yp[0] = yy[0] * (yy[0] - x) / e;
        	break;
        }
    }
    
    private double dsign(double a, double b) {
    	if (b >= 0) {
    		return Math.abs(a);
    	}
    	else {
    		return -Math.abs(a);
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
    		//   on start and restart also set variables x and yy(*), store the
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
    	//   differential equations one step, normally from x to x+h , using a
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
    	//      x  -- independent variable             (real*8)
    	//      y(*) -- solution vector at x          (real*8)
    	//      yp(*)-- derivative of solution vector at  x after successful
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
    	//   the arrays  phi, psi   are required for the interpolation subroutine
    	//   intrp.  the array p  is internal to the code.  all are real*8
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
	    		// compute the g(*) in the vector w(*)
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
    	//   the methods in subroutine  step  approximate the solution near  x 
    	//   by a polynomial.  subroutine  intrp  approximates the solution at
    	//   tout by evaluating the polynomial there.  information defining this
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
    	// yout = y
    	//   and defines
    	//      tout -- point at which solution is desired.
    	//   the remaining parameters are defined in  step  and passed to  intrp
    	//   from that subroutine
    	//
    	//   output from  intrp --
    	//
    	//      yout(*) (y)-- solution at tout
    	//       ypout(*) ) -- derivative of solution at  tout
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