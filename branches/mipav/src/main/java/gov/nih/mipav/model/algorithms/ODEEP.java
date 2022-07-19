package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.util.DoubleDouble;
import gov.nih.mipav.view.Preferences;

public abstract class ODEEP {
	
	 // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    // This is a port of the FORTRAN code ode.f by L. F. Shampine and M. K. Gordon.
    
    //   DoubleDouble precision subroutine ode integrates a system of neqn
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
    //      f -- DoubleDouble precision subroutine f(t,y,yp) to evaluate
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
    //   declare  f  in an external statement, supply the DoubleDouble precision
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
    private DoubleDouble y[];
    private DoubleDouble t;
    private DoubleDouble tout;
    private DoubleDouble relerr;
    private DoubleDouble abserr;
    private int iflag[];
    
    // From work storage in original program:
    private DoubleDouble alpha[] = new DoubleDouble[12];
    private DoubleDouble beta[] = new DoubleDouble[12];
    private DoubleDouble sig[] = new DoubleDouble[13];
    private DoubleDouble v[] = new DoubleDouble[12];
    private DoubleDouble w[] = new DoubleDouble[12];
    private DoubleDouble g[] = new DoubleDouble[13];
    private DoubleDouble phase;
    private DoubleDouble psi[] = new DoubleDouble[12];
    private DoubleDouble x;
    private DoubleDouble h;
    private DoubleDouble hold;
    private DoubleDouble istart;
    private DoubleDouble told;
    private DoubleDouble delsn;
    private DoubleDouble yy[];
    private DoubleDouble wt[];
    private DoubleDouble p[];
    private DoubleDouble yp[];
    private DoubleDouble ypout[];
    private DoubleDouble phi[][];
    
    // From iwork storage in original program
    private int ns;
    private int iwork1;
    private int k;
    private int kold;
    private int isnold;
    
    private boolean start;
    private boolean phase1;
    private boolean nornd;
    
    private DoubleDouble twou;
    private DoubleDouble fouru;
    //   the constant  maxnum  is the maximum number of steps allowed in one
   	//   call to  de .  the user may change this limit by altering the
   	//   following statement
    private int maxnum = 10000000;
    
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
     * Creates a new ODEEP object.
     * Test with:
     * new ODEEPtest();
     * class ODEtest extends ODEEP {
    	  public ODEtest() {
    		  super();
    	  }
    	
    	  public void f(DoubleDouble x, DoubleDouble yy[], DoubleDouble yp[]) {
    		
    	  }
      }
     */
    public ODEEP() {
    	// Passed 36 out of 39 tests
    	// Answers for these 3 are uncertain:
    	// 1.) NONLINEAR_DAMPED_PENDULUM Answer a bit off
    	// 2.) DUFFINGS_WITH_DAMPING_AND_FORCING_FAILED
    	// 3.) POLKINGS_FIRST_ORDER Off 1%
    	// The Lorenz system does not have a well defined stopping point.
        int i;
    	testMode = true;
    	testCase = ENRIGHT_AND_PRYCE_A1;
    	Preferences.debug("Enright andPryce #A1 neqn = 1 y' = -y Exponential decay\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	for (i = 0; i < 10; i++) {
    		y[0] = DoubleDouble.valueOf(1.0);
    		t = DoubleDouble.valueOf(0.0);
    		tout = DoubleDouble.valueOf(2.0*(i+1));
    		relerr = DoubleDouble.valueOf(4.441E-16);
    		abserr = DoubleDouble.valueOf(4.441E-16);
    		
    		iflag[0] = 1;
    		clearArrays();
    		driver();
    		Preferences.debug(getErrorMessage());
    		Preferences.debug("Actual value = " + (tout.negate()).exp() + 
    				" Calculated value = " + y[0] + "\n");
    		Preferences.debug("Final time = " + t+ "\n");
    		Preferences.debug("relerr = " + relerr + " abserr = " + abserr.toString() + "\n");
    	} // for (i = 0; i < 10; i++)
    	
    	testCase = ENRIGHT_AND_PRYCE_A2;
    	Preferences.debug("Enright and Pryce #A2 neqn = 1 y' = -(y^3)/2\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 0.2182178902359887"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_A3;
    	Preferences.debug("Enright and Pryce #A3 neqn = 1 y' = cost(t)*y\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 2.491650271850414E"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_A4;
    	Preferences.debug("Enright and Pryce #A4 neqn = 1 y' = y*(20-y)/80\n");
    	Preferences.debug("y[0] = 1\n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 17.73016648131483"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_A5;
    	Preferences.debug("Enright and Pryce #A5 neqn = 1 y' = (y-t)/(y+t)\n");
    	Preferences.debug("y[0] = 4.0\n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(4.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -0.7887826688964196"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B1;
    	Preferences.debug("Enright and Pryce #B1 neqn = 2 y1' = 2*y1*(1-y2)\n");
    	Preferences.debug("y2' = -y2*(1-y1)\n");
    	Preferences.debug("y[0] = 1.0  y[1] = 3.0\n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	y[1] = DoubleDouble.valueOf(3.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 0.6761876008576667"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 0.1860816099640036"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B2;
    	Preferences.debug("Enright and Pryce #B2 neqn = 3 y1' = -y1 + y2\n");
    	Preferences.debug("y2' = y1 - 2*y2 + y3\n");
    	Preferences.debug("y3' = y2 - y3\n");
    	Preferences.debug("y[0] = 2.0  y[1] = 0.0 y[2] = 1.0\n");
    	neqn = 3;
    	y = new DoubleDouble[3];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(1.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B3;
    	Preferences.debug("Enright and Pryce #B3 neqn = 3 y1' = -y1\n");
    	Preferences.debug("y2' = y1 - y2^2\n");
    	Preferences.debug("y3' = y2^2\n");
    	Preferences.debug("y[0] = 2.0  y[1] = 0.0 y[2] = 1.0\n");
    	neqn = 3;
    	y = new DoubleDouble[3];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B4;
    	Preferences.debug("Enright and Pryce #B4 neqn = 3 y1' = (-y2-y1*y3)/sqrt(y1^2+y2^2)\n");
    	Preferences.debug("y2' = (y1 - y2*y3)/sqrt(y1^2+y2^2)\n");
    	Preferences.debug("y3' = y1/sqrt(y1^2+y2^2)n");
    	Preferences.debug("y[0] = 3.0  y[1] = 0.0 y[2] = 0.0\n");
    	neqn = 3;
    	y = new DoubleDouble[3];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(3.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(8.0E-15);
    	abserr = DoubleDouble.valueOf(8.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_B5;
    	Preferences.debug("Enright and Pryce #B5 neqn = 3 y1' = y2*y3\n");
    	Preferences.debug("y2' = -y1*y3\n");
    	Preferences.debug("y3' = -0.51*y1*y2\n");
    	Preferences.debug("y[0] = 0.0  y[1] = 1.0 y[2] = 1.0\n");
    	// y1 = sn(x, 0.51)
    	// y2 = cn(x, 0.51)
    	// y3 = dn(x, 0.51)
    	neqn = 3;
    	y = new DoubleDouble[3];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.0);
    	y[1] = DoubleDouble.valueOf(1.0);
    	y[2] = DoubleDouble.valueOf(1.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C1;
    	Preferences.debug("Enright and Pryce #C1 neqn = 10 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[9] == 0\n");
    	neqn = 10;
    	y = new DoubleDouble[10];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	for (i = 1; i < 10 ; i++) {
    		y[i] = DoubleDouble.valueOf(0.0);
    	}
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C2;
    	Preferences.debug("Enright and Pryce #C2 neqn = 10 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[9] == 0\n");
    	neqn = 10;
    	y = new DoubleDouble[10];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	for (i = 1; i < 10 ; i++) {
    		y[i] = DoubleDouble.valueOf(0);
    	}
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");	

		testCase = ENRIGHT_AND_PRYCE_C3;
    	Preferences.debug("Enright and Pryce #C3 neqn = 10 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[9] == 0\n");
    	neqn = 10;
    	y = new DoubleDouble[10];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	for (i = 1; i < 10 ; i++) {
    		y[i] = DoubleDouble.valueOf(0.0);
    	}
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C4;
    	Preferences.debug("Enright and Pryce #C4 neqn = 51 \n");
    	Preferences.debug("y[0] = 1\n");
    	Preferences.debug("y[1] to y[50] == 0\n");
    	neqn = 51;
    	y = new DoubleDouble[51];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	for (i = 1; i < 51 ; i++) {
    		y[i] = DoubleDouble.valueOf(0.0);
    	}
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_C5;
    	Preferences.debug("Enright and Pryce #C5 neqn = 30 \n");
    	neqn = 30;
    	y = new DoubleDouble[30];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(3.42947415189);
    	y[1] = DoubleDouble.valueOf(3.35386959711);
    	y[2] = DoubleDouble.valueOf(1.35494901715);
    	y[3] = DoubleDouble.valueOf(6.64145542550);
    	y[4] = DoubleDouble.valueOf(5.97156957878);
    	y[5] = DoubleDouble.valueOf(2.18231499728);
    	y[6] = DoubleDouble.valueOf(11.2630437207);
    	y[7] = DoubleDouble.valueOf(14.6952576794);
    	y[8] = DoubleDouble.valueOf(6.27960525067);
    	y[9] = DoubleDouble.valueOf(-30.1552268759);
    	y[10] = DoubleDouble.valueOf(1.65699966404);
    	y[11] = DoubleDouble.valueOf(1.43785752721);
    	y[12] = DoubleDouble.valueOf(-21.1238353380);
    	y[13] = DoubleDouble.valueOf(28.4465098142);
    	y[14] = DoubleDouble.valueOf(15.3882659679);
    	y[15] = DoubleDouble.valueOf(-.557160570446);
    	y[16] = DoubleDouble.valueOf(.505696783289);
    	y[17] = DoubleDouble.valueOf(.230578543901);
    	y[18] = DoubleDouble.valueOf(-.415570776342);
    	y[19] = DoubleDouble.valueOf(.365682722812);
    	y[20] = DoubleDouble.valueOf(.169143213293);
    	y[21] = DoubleDouble.valueOf(-.325325669158);
    	y[22] = DoubleDouble.valueOf(.189706021964);
    	y[23] = DoubleDouble.valueOf(.0877265322780);
    	y[24] = DoubleDouble.valueOf(-.0240476254170);
    	y[25] = DoubleDouble.valueOf(-.287659532608);
    	y[26] = DoubleDouble.valueOf(-.117219543175);
    	y[27] = DoubleDouble.valueOf(-.176860753121);
    	y[28] = DoubleDouble.valueOf(-.216393453025);
    	y[29] = DoubleDouble.valueOf(-.0148647893090);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(3.0E-15);
    	abserr = DoubleDouble.valueOf(3.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D1;
    	Preferences.debug("Enright and Pryce #D1 neqn = 4 \n");
    	neqn = 4;
    	y = new DoubleDouble[4];
    	iflag = new int[1];
    	allocateArrays();
    	int ID = 31;
    	DoubleDouble E = DoubleDouble.valueOf(.2*((double)ID-30.0) - .1);
    	y[0] = (DoubleDouble.valueOf(1.0)).subtract(E);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	y[3] = (((DoubleDouble.valueOf(1.0)).add(E)).divide(y[0])).sqrt();
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D2;
    	Preferences.debug("Enright and Pryce #D2 neqn = 4 \n");
    	neqn = 4;
    	y = new DoubleDouble[4];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 32;
    	E = DoubleDouble.valueOf(.2*((double)ID-30.0) - .1);
    	y[0] = (DoubleDouble.valueOf(1.0)).subtract(E);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	y[3] = (((DoubleDouble.valueOf(1.0)).add(E)).divide(y[0])).sqrt();
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D3;
    	Preferences.debug("Enright and Pryce #D3 neqn = 4 \n");
    	neqn = 4;
    	y = new DoubleDouble[4];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 33;
    	E = DoubleDouble.valueOf(.2*((double)ID-30.0) - .1);
    	y[0] = (DoubleDouble.valueOf(1.0)).subtract(E);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	y[3] = (((DoubleDouble.valueOf(1.0)).add(E)).divide(y[0])).sqrt();
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D4;
    	Preferences.debug("Enright and Pryce #D4 neqn = 4 \n");
    	neqn = 4;
    	y = new DoubleDouble[4];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 34;
    	E = DoubleDouble.valueOf(.2*((double)ID-30.0) - .1);
    	y[0] = (DoubleDouble.valueOf(1.0)).subtract(E);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	y[3] = (((DoubleDouble.valueOf(1.0)).add(E)).divide(y[0])).sqrt();
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_D5;
    	Preferences.debug("Enright and Pryce #D5 neqn = 4 \n");
    	neqn = 4;
    	y = new DoubleDouble[4];
    	iflag = new int[1];
    	allocateArrays();
    	ID = 35;
    	E = DoubleDouble.valueOf(.2*((double)ID-30.0) - .1);
    	y[0] = (DoubleDouble.valueOf(1.0)).subtract(E);
    	y[1] = DoubleDouble.valueOf(0.0);
    	y[2] = DoubleDouble.valueOf(0.0);
    	y[3] = (((DoubleDouble.valueOf(1.0)).add(E)).divide(y[0])).sqrt();
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E1;
    	Preferences.debug("Enright and Pryce #E1 neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	E = DoubleDouble.valueOf(.79788456080286536);
    	y[0] = E.multiply(DoubleDouble.valueOf(.84147098480789651));
    	y[1] = E.multiply(DoubleDouble.valueOf(.11956681346419146));
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 1.456723600728308D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -9.883500195574063D-02"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E2;
    	Preferences.debug("Enright and Pryce #E2 neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]=  2.008149762174948"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -4.250887527320057D-02"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");

		testCase = ENRIGHT_AND_PRYCE_E3;
    	Preferences.debug("Enright and Pryce #E3 neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -1.004178858647128D-01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 2.411400132095954D-01"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E4;
    	Preferences.debug("Enright and Pryce #E4 neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(30.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 3.395091444646555D+01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 2.767822659672869D-01"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_E5;
    	Preferences.debug("Enright and Pryce #E5 neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.1E-15);
    	abserr = DoubleDouble.valueOf(1.1E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= 1.411797390542629D+01"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= 2.400000000000002"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F1;
    	Preferences.debug("Enright and Pryce #F1 neqn = 2 \n");
    	// Using c1 = PI * Pi + 0.l * 0.1 = 9.8796044010893586199447140245012
    	// In ODE normal return.  Integration reached tout
        // Actual value y[0]= -1.294460621213470D1 Calculated value = -12.944606212134735258130029482344
    	// Actual value y[1]= -2.208575158908672D-15 Calculated value = 6.8000440607758703506550279126979E-14
    	// Using their c1  = 9.879604401089358
    	// In ODE normal return.  Integration reached tout
        // Actual value y[0]= -1.294460621213470D1 Calculated value = -12.944606212134737084682048136178
        // Actual value y[1]= -2.208575158908672D-15 Calculated value = -3.6170938725826828467167589182374E-15
    	// So my y[1] is within the bounds caused by very small changes in c1.
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-30);
    	abserr = DoubleDouble.valueOf(1.0E-30);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value y[0]= -1.294460621213470D1"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value y[1]= -2.208575158908672D-15"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F2;
		Preferences.debug("Enright and Pryce #F2 neqn = 1 \n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(110.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-14);
    	abserr = DoubleDouble.valueOf(1.0E-14);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 70.03731057008607"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F3;
		Preferences.debug("Enright and Pryce #F3 neqn = 2 \n");
		// This is a case where the double passes, but the DoubleDouble fails due to the fTestMode calculation.
		// With x = 20, double calculates sin(PI * 20) as -2.449E-15 which initially puts a
		// nonzero value in yp[1] and allows the algorithm to run.  With x = 20, DoubleDouble calculates
		// sin(PI * 20) as zero, which initially puts a zero value in yp[1] so the algorithm cannot run.
		// Make this algorithm work by varying the tout to a value very slightly different from 20.0.
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.0);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.000000001);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -3.726957553088175D-1"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = -6.230137949234190D-1"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F4;
		Preferences.debug("Enright and Pryce #F4 neqn = 1 \n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 9.815017249707434D-11"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = ENRIGHT_AND_PRYCE_F5;
		Preferences.debug("Enright and Pryce #F5 neqn = 1 \n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 1.0"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = LOTKA_VOLTERRA_PREDATOR_PREY;
		Preferences.debug("Lotka-Volterra Predator-Prey Equations neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(2.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(10.0);
    	relerr = DoubleDouble.valueOf(2.0E-15);
    	abserr = DoubleDouble.valueOf(2.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 2.20050"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 10.2726"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = LORENZ_SYSTEM;
		Preferences.debug("The Lorenz System neqn = 3 \n");
		// The discussion for P32_STOP says:
	    // The system is chaotic, and so a dummy stop value is put here.
	    // so essentially no stop value is given
    	neqn = 3;
    	y = new DoubleDouble[3];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(2.0);
    	y[2] = DoubleDouble.valueOf(21.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-20);
    	abserr = DoubleDouble.valueOf(1.0E-20);
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
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = VAN_DER_POL;
		Preferences.debug("The Van der Pol equation neqn = 2 \n");
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(2.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 0.756245"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 2.67294"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
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
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(2.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-25);
    	abserr = DoubleDouble.valueOf(1.0E-25);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 7.404275751842365E-5"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 2.8894321598544795E-4"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = NONLINEAR_DAMPED_PENDULUM;
		Preferences.debug("The Nonlinear Damped Pendulum neqn = 2 \n");
		// Off a bit:
		// In ODE normal return.  Integration reached tout
		// Actual value = -5.84253E-5 Calculated value = -6.1234679821033738345060488750149E-5
		// Actual value = 3.59969E-4 Calculated value = 3.8103739806483336540659849541942E-4
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(2.0);
    	y[1] = DoubleDouble.valueOf(2.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(20.0);
    	relerr = DoubleDouble.valueOf(1.0E-25);
    	abserr = DoubleDouble.valueOf(1.0E-25);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -5.84253E-5"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 3.59969E-4"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
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
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.5);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(100.0);
    	relerr = DoubleDouble.valueOf(1.0E-20);
    	abserr = DoubleDouble.valueOf(1.0E-20);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 1.18955786581925"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = 0.4417211944847741"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = DUFFINGS_WITH_DAMPING_AND_FORCING;
		Preferences.debug("Duffing's Equation with Damping and Forcing neqn = 2 \n");
		tout = DoubleDouble.valueOf(100.0);
		// From Approximate Solution of Nonlinear Duffing Oscillator Using Taylor
	    // Expansion by A. Okasha El-Nady and Maha M.A.Lashin
		// DoubleDouble delt = DoubleDouble.valueOf(1.0E-6);
		// DoubleDouble deltSquared = delt.multiply(delt);
    	// 0 to tout + delt
    	// int timePositions = (tout.divide(delt)).intValue();
    	// DoubleDouble x[] = new DoubleDouble[timePositions];
    	// DoubleDouble dx[] = new DoubleDouble[timePositions];
    	// DoubleDouble dx2[] = new DoubleDouble[timePositions];
    	// DoubleDouble zero = DoubleDouble.valueOf(0.0);
    	// DoubleDouble p2 = DoubleDouble.valueOf(0.2);
    	// DoubleDouble p3 = DoubleDouble.valueOf(0.3);
    	// DoubleDouble p5 = DoubleDouble.valueOf(0.5);
    	// DoubleDouble two = DoubleDouble.valueOf(2.0);
    	// x[0] = p5;
    	// dx[0] = zero;
    	// dx2[0] = x[0].subtract((x[0].multiply(x[0])).multiply(x[0]));
    	// dx2[0] = dx2[0].subtract(p2.multiply(dx[0]));
    	// dx2[0] = dx2[0].add(p3.multiply(zero.cos()));
    	// x[1] = x[0].add(dx[0].multiply(delt));
    	// x[1] = x[1].add((p5.multiply(dx2[0])).multiply(deltSquared));
    	// x[2] = x[0].add((two.multiply(dx[0])).multiply(delt));
    	// x[2] = x[2].add((two.multiply(dx2[0])).multiply(deltSquared));
    	// dx[1] = (x[2].subtract(x[0])).divide(two.multiply(delt));
    	// dx2[1] = x[1].subtract((x[1].multiply(x[1])).multiply(x[1]));
    	// dx2[1] = dx2[1].subtract(p2.multiply(dx[1]));
    	// dx2[1] = dx2[1].add(p3.multiply(delt.cos()));
    	// for (i = 2; i < timePositions-1; i++) {
    		// x[i] = x[i-1].add(dx[i-1].multiply(delt));
    		// x[i] = x[i].add((p5.multiply(dx2[i-1])).multiply(deltSquared));
    		// x[i+1] = x[i-1].add((two.multiply(dx[i-1])).multiply(delt));
    		// x[i+1] = x[i+1].add((two.multiply(dx2[i-1])).multiply(deltSquared));
    		// dx[i] = (x[i+1].subtract(x[i-1])).divide(two.multiply(delt));
    		// dx2[i] = x[i].subtract((x[i].multiply(x[i])).multiply(x[i]));
        	// dx2[i] = dx2[i].subtract(p2.multiply(dx[i]));
        	// dx2[i] = dx2[i].add(p3.multiply(((DoubleDouble.valueOf(i-1)).multiply(delt)).cos()));
    	// } 
    	// DoubleDouble yout0 = x[timePositions - 2];
    	// System.out.println("yout0 = " + yout0);
    	// DoubleDouble yout1 = dx[timePositions - 2];
    	// System.out.println("yout1 = " + yout1);
    	// Recurrence equations at delt = 1.0E-6 yield
    	// yout0 = -0.90072788840276037178110317710702
        // yout1 = -0.59945614403229628906295032931339
		// Failed:
		// In ODE normal return.  Integration reached tout
		// Actual value = -1.21774 Calculated value = -0.61033142121573199381498839168845
	    // Actual value = -0.548248 Calculated value = 0.21114908857192905940947860835968
    	neqn = 2;
    	y = new DoubleDouble[2];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.5);
    	y[1] = DoubleDouble.valueOf(0.0);
    	t = DoubleDouble.valueOf(0.0);
    	relerr = DoubleDouble.valueOf(1.0E-20);
    	abserr = DoubleDouble.valueOf(1.0E-20);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -1.21774"  + 
				" Calculated value = " + y[0] + "\n");
    	Preferences.debug("Actual value = -0.548248"  + 
				" Calculated value = " + y[1] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = SHAMPINES_BALL_OF_FLAME;
    	Preferences.debug("Shampine's Ball of Flame neqn = 1 y' = y^2-y^3\n");
    	Preferences.debug("y[0] = 1.0E-2\n");
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(1.0E-2);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(200.0);
    	relerr = DoubleDouble.valueOf(1.0E-15);
    	abserr = DoubleDouble.valueOf(1.0E-15);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = 1.00000"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = POLKINGS_FIRST_ORDER;
    	Preferences.debug("Polking's first order ODE neqn = 1 \n");
    	// Off about 1%:
    	// In ODE normal return.  Integration reached tout
    	// Actual value = -3.00000 Calculated value = -2.9715400444864765695057329614553
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(0.5);
    	t = DoubleDouble.valueOf(0.0);
    	tout = DoubleDouble.valueOf(9.0);
    	relerr = DoubleDouble.valueOf(1.0E-25);
    	abserr = DoubleDouble.valueOf(1.0E-25);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -3.00000"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
		
		testCase = KNEE_PROBLEM;
    	Preferences.debug("the Knee problem neqn = 1 \n");
    	//DoubleDouble e = DoubleDouble.valueOf(0.01);
    	//DoubleDouble p5 = DoubleDouble.valueOf(0.5);
    	//DoubleDouble one = DoubleDouble.valueOf(1.0);
    	//DoubleDouble two = DoubleDouble.valueOf(2.0);
    	//DoubleDouble negtwo = two.negate();
    	//DoubleDouble yout = ((negtwo.multiply(e.sqrt()))
    	//        .multiply(( (one.subtract(tout.multiply(tout)) ).divide (two.multiply(e) ) ).exp())).divide
    	//	    ( 
    	//	      (two.multiply( e.sqrt() )).add 
    	//	      ((( 
    	//	       ((one.divide((two.multiply(e) ).sqrt()) ).erf()).add 
    	//	         (( tout.divide((two.add(e) ).sqrt()) ).erf()) 
    	//	      ) 
    	//	      .multiply((p5.divide(e) ).exp())).multiply((DoubleDouble.TWO_PI ).sqrt())) 
    	// 	    );
    	// yout calculated as -0.0
    	neqn = 1;
    	y = new DoubleDouble[1];
    	iflag = new int[1];
    	allocateArrays();
    	y[0] = DoubleDouble.valueOf(-1.0);
    	t = DoubleDouble.valueOf(-1.0);
    	tout = DoubleDouble.valueOf(1.0);
    	relerr = DoubleDouble.valueOf(1.0E-37);
    	abserr = DoubleDouble.valueOf(1.0E-37);
    	iflag[0] = 1;
    	clearArrays();
    	driver();
    	Preferences.debug(getErrorMessage());
    	Preferences.debug("Actual value = -0.0"  + 
				" Calculated value = " + y[0] + "\n");
		Preferences.debug("Final time = " + t + "\n");
		Preferences.debug("relerr = " + relerr + " abserr = " + abserr + "\n");
    	
    }
    
    private void allocateArrays() {
    	 yy = new DoubleDouble[neqn];
         wt = new DoubleDouble[neqn];
         p = new DoubleDouble[neqn];
         yp = new DoubleDouble[neqn];
         ypout = new DoubleDouble[neqn];
         phi = new DoubleDouble[neqn][16];	
    }
    
    private void clearArrays() {
    	int i;
    	int j;
    	for (i = 0; i < 12; i++) {
    		alpha[i] = DoubleDouble.valueOf(0.0);
    		beta[i] =  DoubleDouble.valueOf(0.0);
    		v[i] =  DoubleDouble.valueOf(0.0);
    		w[i] =  DoubleDouble.valueOf(0.0);
    		psi[i] =  DoubleDouble.valueOf(0.0);
    	}
    	for (i = 0; i < 13; i++) {
    		sig[i] =  DoubleDouble.valueOf(0.0);
    		g[i] =  DoubleDouble.valueOf(0.0);
    	}
    	for (i = 0; i < neqn; i++) {
    		yy[i] =  DoubleDouble.valueOf(0.0);
    		wt[i] =  DoubleDouble.valueOf(0.0);
    		p[i] =  DoubleDouble.valueOf(0.0);
    		yp[i] =  DoubleDouble.valueOf(0.0);
    		ypout[i] =  DoubleDouble.valueOf(0.0);
    		for (j = 0; j < 16; j++) {
    			phi[i][j] =  DoubleDouble.valueOf(0.0);
    		}
    	}
    }
    
    private void fTestMode(DoubleDouble x, DoubleDouble yy[], DoubleDouble yp[]) {
    	int i;
    	int i3;
    	int i3m2;
    	int itemp;
    	int j;
    	int l;
    	int ll;
    	int mm;
    	DoubleDouble a;
    	DoubleDouble b;
    	DoubleDouble c;
    	DoubleDouble d;
    	DoubleDouble denom;
    	DoubleDouble diff1;
    	DoubleDouble diff2;
    	DoubleDouble diff3;
    	DoubleDouble p;
    	DoubleDouble Q[][];
    	DoubleDouble r[];
    	DoubleDouble temp;
    	DoubleDouble var;
    	DoubleDouble g = DoubleDouble.valueOf(32.0);
    	DoubleDouble length =  DoubleDouble.valueOf(1.0);
    	DoubleDouble dampingCoefficient =  DoubleDouble.valueOf(1.0);
    	DoubleDouble mass =  DoubleDouble.valueOf(1.0);
        switch(testCase) {
        case ENRIGHT_AND_PRYCE_A1:
        	yp[0] = yy[0].negate();
        	break;
        case ENRIGHT_AND_PRYCE_A2:
        	yp[0] = ((yy[0].pow(3.0)).divide(DoubleDouble.valueOf(2.0))).negate();
        	break;
        case ENRIGHT_AND_PRYCE_A3:
        	yp[0] = (x.cos()).multiply(yy[0]);
        	break;
        case ENRIGHT_AND_PRYCE_A4:
        	yp[0] = (yy[0].multiply((DoubleDouble.valueOf(20.0)).subtract(yy[0]))).
        			divide(DoubleDouble.valueOf(80.0));
        	break;
        case ENRIGHT_AND_PRYCE_A5:
        	yp[0] = (yy[0].subtract(x)).divide(yy[0].add(x));
        	break;
        case ENRIGHT_AND_PRYCE_B1:
        	yp[0] = ((DoubleDouble.valueOf(2.0)).multiply(yy[0])).
        			multiply((DoubleDouble.valueOf(1.0)).subtract(yy[1]));
        	yp[1] = (yy[1].multiply((DoubleDouble.valueOf(1.0)).subtract(yy[0]))).negate();
        	break;
        case ENRIGHT_AND_PRYCE_B2:
        	yp[0] = (yy[0].negate()).add(yy[1]);
        	yp[1] = (yy[0].subtract((DoubleDouble.valueOf(2.0)).multiply(yy[1]))).add(yy[2]);
        	yp[2] = yy[1].subtract(yy[2]);
        	break;
        case ENRIGHT_AND_PRYCE_B3:
        	yp[0] = yy[0].negate();
        	var = yy[1].multiply(yy[1]);
        	yp[1] = yy[0].subtract(var);
        	yp[2] = var; 
        	break;
        case ENRIGHT_AND_PRYCE_B4:
            denom = ((yy[0].multiply(yy[0])).add(yy[1].multiply(yy[1]))).sqrt();
            yp[0] = (yy[1].negate()).subtract((yy[0].multiply(yy[2])).divide(denom));
            yp[1] = yy[0].subtract((yy[1].multiply(yy[2])).divide(denom));
            yp[2] = yy[0].divide(denom);
            break;
        case ENRIGHT_AND_PRYCE_B5:
        	yp[0] = yy[1].multiply(yy[2]);
        	yp[1] = (yy[0].negate()).multiply(yy[2]);
        	yp[2] = ((DoubleDouble.valueOf(-0.51)).multiply(yy[0])).multiply(yy[1]);
        	break;
        case ENRIGHT_AND_PRYCE_C1:
            yp[0] = yy[0].negate();
        	for (i = 1; i <= 8; i++) {
        		yp[i] = yy[i-1].subtract(yy[i]);
        	}
        	yp[9] = yy[8];
        	break;
        case ENRIGHT_AND_PRYCE_C2:
            yp[0] = yy[0].negate();
        	for (i = 2; i <= 9; i++) {
        		yp[i-1] = ((DoubleDouble.valueOf(i-1.0)).multiply(yy[i-2])).subtract
        				((DoubleDouble.valueOf(i)).multiply(yy[i-1]));
        	}
        	yp[9] = (DoubleDouble.valueOf(9.0)).multiply(yy[8]);
        	break;
        case ENRIGHT_AND_PRYCE_C3:
            yp[0] = ((DoubleDouble.valueOf(-2.0)).multiply(yy[0])).add(yy[1]);
        	for (i = 1; i <= 8; i++) {
        		yp[i] = (yy[i-1].subtract((DoubleDouble.valueOf(2.0)).multiply(yy[i]))).add(yy[i+1]);
        	}
        	yp[9] = yy[8].subtract((DoubleDouble.valueOf(2.0)).multiply(yy[9]));
            break;
        case ENRIGHT_AND_PRYCE_C4:
            yp[0] = ((DoubleDouble.valueOf(-2.0)).multiply(yy[0])).add(yy[1]);
        	for (i = 1; i <= 49; i++) {
        		yp[i] = (yy[i-1].subtract((DoubleDouble.valueOf(2.0)).multiply(yy[i]))).add(yy[i+1]);
        	}
        	yp[50] = yy[49].subtract((DoubleDouble.valueOf(2.0)).multiply(yy[50]));
            break;
        case ENRIGHT_AND_PRYCE_C5:
        	r = new DoubleDouble[5];
        	Q = new DoubleDouble[5][5];
        	// THE FOLLOWING DATA IS FOR PROBLEM C5 AND DEFINES THE MASSES
        	// OF THE 5 OUTER PLANETS ETC. IN SOLAR UNITS.
        	// K2 IS THE GRAVITATIONAL CONSTANT.

        	DoubleDouble m0 = DoubleDouble.valueOf(1.00000597682);
        	DoubleDouble m[] = new DoubleDouble[]{DoubleDouble.valueOf(.954786104043E-3),
        			DoubleDouble.valueOf(.285583733151E-3), DoubleDouble.valueOf(.437273164546E-4),
        			DoubleDouble.valueOf(.517759138449E-4), DoubleDouble.valueOf(.277777777778E-5)};
        	DoubleDouble k2 = DoubleDouble.valueOf(2.95912208286);
        	i = 0;
            for (l = 3; l <= 15; l +=3) {
               i = i + 1;
               p = ((yy[l-3].multiply(yy[l-3])).add(yy[l-2].multiply(yy[l-2]))).
            		   add(yy[l-1].multiply(yy[l-1]));
               r[i-1] = (DoubleDouble.valueOf(1.0)).divide(p.multiply(p.sqrt()));
               j = 0;
               for (ll = 3; ll <= 15; ll += 3) {
                  j = j + 1;
                  if (ll == l) {
                	  continue;
                  }
                  diff1 = yy[l-3].subtract(yy[ll-3]);
                  diff2 = yy[l-2].subtract(yy[ll-2]);
                  diff3 = yy[l-1].subtract(yy[ll-1]);
                  p = ((diff1.multiply(diff1)).add(diff2.multiply(diff2))).
                		  add(diff3.multiply(diff3));
                  Q[i-1][j-1] = (DoubleDouble.valueOf(1.0)).divide(p.multiply(p.sqrt()));
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
                  p = DoubleDouble.valueOf(0.0);
                  for (j = 1; j <= 5; j++) {
                     mm = mm + 3;
                     if (j != i) { 
                    	 p = p.add(m[j-1].multiply((yy[mm-1].multiply(Q[i-1][j-1].subtract(r[j-1])))
                    			 .subtract(yy[ll-1].multiply(Q[i-1][j-1]))));
                     } // if (j != i)
                  } // for (j = 1; j <= 5; j++)
                  yp[ll+14] = k2.multiply(((((m0.add(m[i-1])).multiply(yy[ll-1]))
                		  .multiply(r[i-1])).negate()).add(p));
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
        	denom = (yy[0].multiply(yy[0])).add(yy[1].multiply(yy[1]));
        	denom = denom.pow(1.5);
        	yp[2] = (yy[0].negate()).divide(denom);
        	yp[3] = (yy[1].negate()).divide(denom);
        	break;
        case ENRIGHT_AND_PRYCE_E1:
            var = x.add(DoubleDouble.valueOf(1.0));
            yp[0] = yy[1];
            yp[1] = (DoubleDouble.valueOf(0.25)).divide(var.multiply(var));
            yp[1] = ((DoubleDouble.valueOf(1.0)).subtract(yp[1])).multiply(yy[0]);
            yp[1] = ((yy[1].divide(var)).add(yp[1])).negate();
        	break;
        case ENRIGHT_AND_PRYCE_E2:
            yp[0] = yy[1];
            yp[1] = (DoubleDouble.valueOf(1.0)).subtract(yy[0].multiply(yy[0]));
            yp[1] = yp[1].multiply(yy[1]);
            yp[1] = yp[1].subtract(yy[0]);
            break;
        case ENRIGHT_AND_PRYCE_E3:
            yp[0] = yy[1];
        	yp[1]= ((((yy[0].multiply(yy[0])).multiply(yy[0])).divide(DoubleDouble.valueOf(6.0))).subtract(yy[0])).
        			add((DoubleDouble.valueOf(2.0)).multiply(((DoubleDouble.valueOf(2.78535)).multiply(x)).sin()));
            break;
        case ENRIGHT_AND_PRYCE_E4:
            yp[0] = yy[1];
            yp[1] = (DoubleDouble.valueOf(0.032)).subtract(((DoubleDouble.valueOf(0.4)).multiply(yy[1])).multiply(yy[1]));
            break;
        case ENRIGHT_AND_PRYCE_E5:
            yp[0] = yy[1];
        	//yp[1] = Math.sqrt(1.0 + yy[1]*yy[1])/(25.0-x);
        	yp[1] = (((DoubleDouble.valueOf(1.0)).add(yy[1].multiply(yy[1]))).sqrt())
        			.divide((DoubleDouble.valueOf(25.0)).subtract(x));
        	break;
        case ENRIGHT_AND_PRYCE_F1:
        	// C1 IS PI**2 + 0.1**2 
        	DoubleDouble c1 = ((DoubleDouble.PI).multiply(DoubleDouble.PI)).
        	add(DoubleDouble.valueOf(0.1).multiply(DoubleDouble.valueOf(0.1)));
            yp[0] = yy[1];
        	yp[1] = ((DoubleDouble.valueOf(0.2)).multiply(yy[1]) ).subtract(c1.multiply(yy[0]));
        	itemp = (x.floor()).intValue();
        	if ((itemp/2)*2 != itemp) {
        		yp[1] = yp[1].subtract(DoubleDouble.valueOf(1.0));
        	}
        	else {
        		yp[1] = yp[1].add(DoubleDouble.valueOf(1.0));
        	}
        	break;
        case ENRIGHT_AND_PRYCE_F2:
        	itemp = (x.floor()).intValue();
        	if ((itemp/2)*2 != itemp) {
        		yp[0] = (DoubleDouble.valueOf(55.0)).subtract((DoubleDouble.valueOf(0.5)).multiply(yy[0]));
        	}
        	else {
        		yp[0] = (DoubleDouble.valueOf(55.0)).subtract((DoubleDouble.valueOf(1.5)).multiply(yy[0]));
        	}
        	break;
        case ENRIGHT_AND_PRYCE_F3:
            yp[0] = yy[1];
            yp[1] = (DoubleDouble.valueOf(0.01)).multiply(yy[1]);
            yp[1] = yp[1].multiply((DoubleDouble.valueOf(1.0)).subtract(yy[0].multiply(yy[0])));
            yp[1] = yp[1].subtract(yy[0]);
            yp[1] = yp[1].subtract((((DoubleDouble.PI).multiply(x)).sin()).abs());
        	break;
        case ENRIGHT_AND_PRYCE_F4:
            if (x.le(DoubleDouble.valueOf(10.0))) {
                temp = x.subtract(DoubleDouble.valueOf(5.0));	
                yp[0] = ((DoubleDouble.valueOf(-2.0)).divide(DoubleDouble.valueOf(21.0))).
                		subtract(((DoubleDouble.valueOf(120.0)).multiply(temp)).
                				divide(((DoubleDouble.valueOf(1.0)).
                				add(((DoubleDouble.valueOf(4.0)).multiply(temp)).multiply(temp))).pow(16)));
            }
            else {
            	yp[0] = (DoubleDouble.valueOf(-2.0)).multiply(yy[0]);
            }
        	break;
        case ENRIGHT_AND_PRYCE_F5:
        	DoubleDouble ex = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(3.0));
        	// C2 IS SUM I**(4/3) FOR I=1 TO 19.
        	DoubleDouble c2 = DoubleDouble.valueOf(0.0);
        	for (i = 1; i <= 19; i++) {
        		c2 = c2.add((DoubleDouble.valueOf(i)).pow((DoubleDouble.valueOf(4.0)).divide(DoubleDouble.valueOf(3.0))));
        	}
        	DoubleDouble xm[]= new DoubleDouble[20];
        	for (i = 1; i <= 19; i++) {
        	   xm[i] = x.subtract(DoubleDouble.valueOf(i));	
        	}
        	DoubleDouble xp[] = new DoubleDouble[20];
        	for (i = 1; i <= 19; i++) {
        		xp[i] = (xm[i].abs()).pow(ex);
        	}
        	
        	DoubleDouble val = (DoubleDouble.valueOf(4.0)).divide((DoubleDouble.valueOf(3.0)).multiply(c2));
        	DoubleDouble sum = DoubleDouble.valueOf(0.0);
        	for (i = 1; i <= 19; i++) {
        	    sum = sum.add(dsign(xp[i],xm[i]));	
        	}
        	yp[0] = (yy[0].multiply(val)).multiply(sum);
        	break;
        case LOTKA_VOLTERRA_PREDATOR_PREY:
        	a = DoubleDouble.valueOf(5.0);
        	b = DoubleDouble.valueOf(1.0);
        	c = DoubleDouble.valueOf(0.5);
        	d = DoubleDouble.valueOf(2.0);
        	yp[0] = (a.subtract(b.multiply(yy[1]))).multiply(yy[0]);
        	yp[1] = ((c.multiply(yy[0])).subtract(d)).multiply(yy[1]);
        	break;
        case LORENZ_SYSTEM:
        	DoubleDouble sigma = DoubleDouble.valueOf(10.0);
        	DoubleDouble rho = DoubleDouble.valueOf(28.0);
        	DoubleDouble beta = (DoubleDouble.valueOf(8.0)).divide(DoubleDouble.valueOf(3.0));
            yp[0] = sigma.multiply(yy[1].subtract(yy[0]));
        	yp[1] = ((rho.multiply(yy[0])).subtract(yy[1])).subtract(yy[0].multiply(yy[2]));
        	yp[2] = ((beta.negate()).multiply(yy[2])).add(yy[0].multiply(yy[1]));
        	break;
        case VAN_DER_POL:
        	DoubleDouble delta = DoubleDouble.valueOf(1.0);
        	yp[0] = yy[1];
        	yp[1] = ((delta.multiply((DoubleDouble.valueOf(1.0)).subtract(yy[0].multiply(yy[0])))).multiply(yy[1])).subtract(yy[0]);
        	break;
        case LINEARIZED_DAMPED_PENDULUM:
            yp[0] = yy[1];
        	yp[1] = (((g.divide(length)).negate()).multiply(yy[0])).
        			subtract((dampingCoefficient.divide(mass)).multiply(yy[1]));
        	break;
        case NONLINEAR_DAMPED_PENDULUM:
        	yp[0] = yy[1];
        	yp[1] = (((g.divide(length)).negate()).multiply(yy[0].sin())).
        			subtract((dampingCoefficient.divide(mass)).multiply(yy[1]));
        	break;
        case DUFFINGS:
        	yp[0] = yy[1];
        	yp[1] = yy[0].multiply((DoubleDouble.valueOf(1.0)).subtract(yy[0].multiply(yy[0])));
        	break;
        case DUFFINGS_WITH_DAMPING_AND_FORCING:
        	DoubleDouble amplitude = DoubleDouble.valueOf(0.3);
        	DoubleDouble kdamp = DoubleDouble.valueOf(0.2);
        	DoubleDouble w = DoubleDouble.valueOf(1.0);
        	yp[0] = yy[1];
        	yp[1] = yy[0].multiply((DoubleDouble.valueOf(1.0)).subtract(yy[0].multiply(yy[0])));
        	yp[1] = yp[1].subtract(kdamp.multiply(yy[1]));
        	yp[1] = yp[1].add(amplitude.multiply((w.multiply(x)).cos()));
        	break;
        case SHAMPINES_BALL_OF_FLAME:
        	DoubleDouble yySquared;
        	yySquared = yy[0].multiply(yy[0]);
        	yp[0] = yySquared.subtract(yySquared.multiply(yy[0]));
        	break;
        case POLKINGS_FIRST_ORDER:
        	a = DoubleDouble.valueOf(1.0);
        	b = DoubleDouble.valueOf(0.0);
        	yp[0] = ((yy[0].multiply(yy[0])).subtract(a.multiply(x))).add(b);
        	break;
        case KNEE_PROBLEM:
        	DoubleDouble e = DoubleDouble.valueOf(0.01);
        	yp[0] = (yy[0].multiply(yy[0].subtract(x))).divide(e);
        	break;
        }
    }
    
    private DoubleDouble dsign(DoubleDouble a, DoubleDouble b) {
    	if (b.ge(DoubleDouble.valueOf(0.0))) {
    		return a.abs();
    	}
    	else {
    		return (a.abs()).negate();
    	}
    }
    
    // input neqn = number of equations to be integrated
    // input/output DoubleDouble y[] = new DoubleDouble[neqn]
    // y input contains y values at starting time
    // y output contains y values for last point reached in integration, normally tout.
    // input/output DoubleDouble t[] = new DoubleDouble[1]
    // t input contains the starting point of integration
    // t output the last point reached in integration.  Normal return has t = tout.
    // input tout has point at which solution is desired
    // input/ouput DoubleDouble relerr[] = new DoubleDouble[1]
    // input has starting relative error tolerance
    // output with normal return has relerr unchanged, but relerr is increased for iflag = 3
    // input/ouput DoubleDouble abserr[] = new DoubleDouble[1]
    // input has starting absolute error tolerance
    // output with normal return has abserr unchanged, but abserr is increased for iflag = 3
    // input/output int iflag[] = new int[1] iflag[0] = 1
    // Input is +1 or -1.  Indicator to initialize the code.  Normal input is +1.
    // The user should set iflag[0] = -1 only if it is impossible to continue
    // integration beyond tout.
    // iflag output as described above.
    public ODEEP(int neqn, DoubleDouble y[], DoubleDouble t, DoubleDouble tout, DoubleDouble relerr,
    		DoubleDouble abserr, int iflag[]) {
    	        this.neqn = neqn;
        this.y = y;
        this.t = t;
        this.tout = tout;
        this.relerr = relerr;
        this.abserr = abserr;
        this.iflag = iflag;
        yy = new DoubleDouble[neqn];
        wt = new DoubleDouble[neqn];
        p = new DoubleDouble[neqn];
        yp = new DoubleDouble[neqn];
        ypout = new DoubleDouble[neqn];
        phi = new DoubleDouble[neqn][16];
    }
    
    public DoubleDouble[] getY() {
    	return y;
    }
    public DoubleDouble getT() {
    	return t;
    }
    
    public int getIflag() {
        return iflag[0];	
    }
    
    public DoubleDouble getRelerr() {
        return relerr;	
    }
    
    public String getErrorMessage() {
    	String message = null;
    	if (iflag[0] == 2) {
    		message = new String("In ODE normal return.  Integration reached tout\n");
    	}
    	else if (iflag[0] == 3) {
    		message = new String("In ODE integration did not reach tout because error\n" +
    			    "tolerances too small.  relerr increased to " + relerr + "\n" +
    				"abserr increased to " + abserr + "\n");
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
    
    public DoubleDouble getAbserr() {
    	return abserr;
    }
     public void driver() {   
    	
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - DoubleDoubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        DoubleDouble epsilon = DoubleDouble.valueOf(1.0);
        DoubleDouble neweps = DoubleDouble.valueOf(1.0);

        while (true) {

            if ((DoubleDouble.valueOf(1.0)).equals((DoubleDouble.valueOf(1.0)).add(neweps))) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps.divide(DoubleDouble.valueOf(2.0));
            }
        } // while(true)
        
        twou = (DoubleDouble.valueOf(2.0)).multiply(epsilon);
        fouru = (DoubleDouble.valueOf(4.0)).multiply(epsilon);

        if (Math.abs(iflag[0]) != 1) {
            start = (istart.gt(DoubleDouble.valueOf(0.0)));
            phase1 = (phase.gt(DoubleDouble.valueOf(0.0)));
            nornd = iwork1 != -1;
        }
       
        de();
        
        istart = DoubleDouble.valueOf(-1.0);
        if (start) {
        	istart = DoubleDouble.valueOf(1.0);
        }
        phase = DoubleDouble.valueOf(-1.0);
        if (phase1) {
        	phase = DoubleDouble.valueOf(1.0);
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
    	DoubleDouble eps[] = new DoubleDouble[1];
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
        if (t.equals(tout)) {
        	iflag[0] = 6;
        	return;
        }
    	if ((relerr.lt(DoubleDouble.valueOf(0.0))) || (abserr.lt(DoubleDouble.valueOf(0.0)))) {
    		iflag[0] = 6;
    		return;
    	}
    	eps[0] = (relerr).max(abserr);
    	if (eps[0].le(DoubleDouble.valueOf(0.0))) {
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
    		if (t.ne(told)) {
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
    	
    	DoubleDouble del = tout.subtract(t);
    	DoubleDouble absdel = del.abs();
    	DoubleDouble tend = t.add((DoubleDouble.valueOf(10.0)).multiply(del));
    	if (isn < 0) {
    		tend = tout;
    	}
    	int nostep = 0;
    	int kle4 = 0;
    	stiff = false;
    	DoubleDouble releps = relerr.divide(eps[0]);
    	DoubleDouble abseps = abserr.divide(eps[0]);
    	if ((iflag[0] == 1) || (isnold < 0) || ((delsn.multiply(del)).le(DoubleDouble.valueOf(0.0)))) {
    		//
    		//   on start and restart also set variables x and yy(*), store the
    		//   direction of integration and initialize the step size
    		//
    		start = true;
    		x = t;
    		for (l = 0; l < neqn; l++) {
    			yy[l] = y[l];
    		}
    		if (del.ge(DoubleDouble.valueOf(0.0))) {
    		   delsn = DoubleDouble.valueOf(1.0);	
    		}
    		else {
    			delsn = DoubleDouble.valueOf(-1.0);
    		}
    		DoubleDouble maxVal = ((tout.subtract(x)).abs()).max(fouru.multiply(x.abs()));
    		if ((tout.subtract(x)).ge(DoubleDouble.valueOf(0.0))) {
    			h = maxVal;
    		}
    		else {
    			h = maxVal.negate();
    		}
    	} // if ((iflag[0] == 1) || (isnold < 0) || (delsn*del <= 0.0))
    	//
    	// if already past output point, interpolate and return
    	//
    	while (true) {
	    	if (((x.subtract(t)).abs()).ge(absdel)) {
	    	    intrp();
	    	    iflag[0] = 2;
	    	    t = tout;
	    	    told = t;
	    	    isnold = isn;
	    	    return;
	    	} // if (Math.abs(x - t) >= absdel)
	    	//
	    	// if cannot go past output point and sufficiently close,
	    	// extrapolate and return
	    	//
	    	if ((isn <= 0) && (((tout.subtract(x)).abs()).lt(fouru.multiply(x.abs())))) {
	    	    h = tout.subtract(x);	
	    	    if (testMode) {
	    	    	fTestMode(x, yy, yp);
	    	    }
	    	    else {
	    	        f(x, yy, yp);
	    	    }
	    	    for (l = 0; l < neqn; l++) {
	    	        y[l] = yy[l].add(h.multiply(yp[l]));
	    	    }
	    	    iflag[0] = 2;
	    	    t = tout;
	    	    told = t;
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
	    		t = x;
	    		told = t;
	    		isnold = 1;
	    		return;
	    	} // if (nostep >= maxnum)
	    	//
	    	// limit step size, set weight vector and take a step
	    	//
	    	DoubleDouble minVal = (h.abs()).min((tend.subtract(x)).abs());
	    	if (h.ge(DoubleDouble.valueOf(0.0))) {
	    		h = minVal;
	    	}
	    	else {
	    		h = minVal.negate();
	    	}
	    	for (l = 0; l < neqn; l++) {
	    		wt[l] = (releps.multiply(yy[l].abs())).add(abseps);
	    	}
	    	step(eps, crash);
	    	//
	    	// test for tolerances not too small
	    	//
	    	if (crash[0]) {
	    		iflag[0] = isn * 3;
	    		relerr = eps[0].multiply(releps);
	    		abserr = eps[0].multiply(abseps);
	    		for (l = 0; l < neqn; l++) {
	    		    y[l] = yy[l];	
	    		}
	    		t = x;
	    		told = t;
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
    
    public void step(DoubleDouble eps[], boolean crash[]) {
    	//
    	//   DoubleDouble precision subroutine  step
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
    	DoubleDouble absh;
    	DoubleDouble erk;
    	DoubleDouble erkm1;
    	DoubleDouble erkm2;
    	DoubleDouble erkp1;
    	DoubleDouble err;
    	DoubleDouble hnew;
    	DoubleDouble r;
    	DoubleDouble rho;
    	DoubleDouble tau;
    	DoubleDouble temp1;
    	DoubleDouble temp2;
    	DoubleDouble temp3;
    	DoubleDouble temp4;
    	DoubleDouble temp5;
    	DoubleDouble temp6;
    	DoubleDouble var;
    	DoubleDouble xold;
    	DoubleDouble gstr[] = new DoubleDouble[]{DoubleDouble.valueOf(0.500),DoubleDouble.valueOf(0.0833),
    			DoubleDouble.valueOf(0.0417),DoubleDouble.valueOf(0.0264),DoubleDouble.valueOf(0.0188),
    			DoubleDouble.valueOf(0.0143),DoubleDouble.valueOf(0.0114),DoubleDouble.valueOf(0.00936),
    			DoubleDouble.valueOf(0.00789),DoubleDouble.valueOf(0.00679),DoubleDouble.valueOf(0.00592),
    			DoubleDouble.valueOf(0.00524),DoubleDouble.valueOf(0.00468)};
    	DoubleDouble two[] = new DoubleDouble[]{DoubleDouble.valueOf(2.0),DoubleDouble.valueOf(4.0),
    			DoubleDouble.valueOf(8.0),DoubleDouble.valueOf(16.0),DoubleDouble.valueOf(32.0),
    			DoubleDouble.valueOf(64.0),DoubleDouble.valueOf(128.0),DoubleDouble.valueOf(256.0),
    			DoubleDouble.valueOf(512.0),DoubleDouble.valueOf(1024.0),DoubleDouble.valueOf(2048.0),
    			DoubleDouble.valueOf(4096.0),DoubleDouble.valueOf(8192.0)};
  
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
    	if ((h.abs()).lt(fouru.multiply(x.abs()))) {
    		if (h.ge(DoubleDouble.valueOf(0.0))) {
    			h = fouru.multiply(x.abs());
    		}
    		else {
    			h = (fouru.multiply(x.abs())).negate();
    		}
    		return;
    	} // if (Math.abs(h]) < fouru*Math.abs(x))
    	DoubleDouble p5eps = (DoubleDouble.valueOf(0.5)).multiply(eps[0]);
    	//
    	// if step size is too small, determine an acceptable one
    	//
    	DoubleDouble round = DoubleDouble.valueOf(0.0);
    	for (l = 0; l < neqn; l++) {
    		var = (yy[l].divide(wt[l]));
    	    round = round.add(var.multiply(var));	
    	} // for (l = 0; l < neqn; l++)
    	round = twou.multiply(round.sqrt());
    	if (p5eps.lt(round)) {
    		eps[0] = ((DoubleDouble.valueOf(2.0)).multiply(round)).
    				multiply((DoubleDouble.valueOf(1.0)).add(fouru));
    		return;
    	} // if (p5eps < round)
    	crash[0] = false;
    	g[0] = DoubleDouble.valueOf(1.0);
    	g[1] = DoubleDouble.valueOf(0.5);
    	sig[0] = DoubleDouble.valueOf(1.0);
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
    	    DoubleDouble sum = DoubleDouble.valueOf(0.0);
    	    for (l = 0; l < neqn; l++) {
    	    	phi[l][0] = yp[l];
    	    	phi[l][1] = DoubleDouble.valueOf(0.0);
    	    	var = (yp[l].divide(wt[l]));
    	    	sum = sum.add(var.multiply(var));
    	    } // for (l = 0; l < neqn; l++)
    	    sum = sum.sqrt();
    	    absh = h.abs();
    	    if (eps[0].lt((((DoubleDouble.valueOf(16.0)).multiply(sum)).multiply(h)).multiply(h))) {
    	    	absh = (DoubleDouble.valueOf(0.25)).multiply((eps[0].divide(sum)).sqrt());
    	    }
    	    if (h.ge(DoubleDouble.valueOf(0.0))) {
    	    	h = absh.max(fouru.multiply(x.abs()));
    	    }
    	    else {
    	    	h = (absh.max(fouru.multiply(x.abs()))).negate();
    	    }
    	    hold = DoubleDouble.valueOf(0.0);
    	    k = 1;
    	    kold = 0;
    	    start = false;
    	    phase1 = true;
    	    nornd = true;
    	    if (p5eps.le((DoubleDouble.valueOf(100.0)).multiply(round))) {
    	        nornd = false;
    	        for (l = 0; l < neqn; l++) {
    	        	phi[l][14] = DoubleDouble.valueOf(0.0);
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
	    	if (h.ne(hold)) {
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
	    		beta[ns - 1] = DoubleDouble.valueOf(1.0);
	    		DoubleDouble realns = DoubleDouble.valueOf(ns);
	    		alpha[ns - 1] = realns.reciprocal();
	    		temp1 = h.multiply(realns);
	    		sig[nsp1 - 1] = DoubleDouble.valueOf(1.0);
	    		if (k >= nsp1) {
	    		    for (i = nsp1; i <= k; i++) {
	    		        int im1 = i - 1;
	    		        temp2 = psi[im1 - 1];
	    		        psi[im1 -1] = temp1;
	    		        beta[i - 1] = (beta[im1-1].multiply(psi[im1-1])).divide(temp2);
	    		        temp1 = temp2.add(h);
	    		        alpha[i - 1] = h.divide(temp1);
	    		        DoubleDouble reali = DoubleDouble.valueOf(i);
	    		        sig[i] = (reali.multiply(alpha[i - 1])).multiply(sig[i - 1]);
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
	    		        temp3 = DoubleDouble.valueOf(iq*(iq+1));
	    		        v[iq - 1] = temp3.reciprocal();
	    		        w[iq - 1] = v[iq - 1];
	    		    } // for (iq = 1; iq <= k; iq++)
	    		} // if (ns <= 1)
	    		else {
	    			//
	    			// if order was raised, update diagonal part of v(*)
	    			//
		    		if (k > kold) {
		    		    temp4 = DoubleDouble.valueOf(k*kp1);
		    		    v[k - 1] = temp4.reciprocal();
		    		    int nsm2 = ns - 2;
		    		    if (nsm2 >= 1) {
		    		    	for (j = 1; j <= nsm2; j++) {
		    		    	    i = k - j;
		    		    	    v[i - 1] = v[i - 1].subtract(alpha[j].multiply(v[i]));
		    		    	} // for (j = 1; j <= nsm2; j++)
		    		    } // if (nsm2 >= 1)
		    		} // if (k > kold)
		    		//
		    		// update v(8) and set w(*)
		    		//
		    		int limit1 = kp1 - ns;
		    		temp5 = alpha[ns - 1];
		    		for (iq = 1; iq <= limit1; iq++) {
		    			v[iq - 1] = v[iq - 1].subtract(temp5.multiply(v[iq]));
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
	    		    	    w[iq - 1] = w[iq - 1].subtract(temp6.multiply(w[iq]));	
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
	    		        phi[l-1][i-1] = temp1.multiply(phi[l-1][i-1]);	
	    		    } // for (l = 1; l <= neqn; l++)
	    		} // for (i = nsp1; i <= k; i++)
	    	} // if (k >= nsp1)
	    	//
	    	// predict solution and differences
	    	//
	    	for (l = 1; l <= neqn; l++) {
	    	    phi[l-1][kp2-1]	 = phi[l-1][kp1-1];
	    	    phi[l-1][kp1-1] = DoubleDouble.valueOf(0.0);
	    	    p[l - 1] = DoubleDouble.valueOf(0.0);
	    	} // for (l = 1; l <= neqn; l++)
	    	for (j = 1; j <= k; j++) {
	    	    i = kp1 - j;
	    	    ip1 = i+1;
	    	    temp2 = g[i - 1];
	    	    for (l = 1; l <= neqn; l++) {
	    	        p[l - 1] = p[l - 1].add(temp2.multiply(phi[l-1][i-1]));
	    	        phi[l-1][i-1] = phi[l-1][i-1].add(phi[l-1][ip1-1]);
	    	    } // for (l = 1; l <= neqn; l++)
	    	} // for (j = 1; j <= k; j++)
	    	if (!nornd) {
	    		for (l = 1; l <= neqn; l++) {
	    		    tau = (h.multiply(p[l - 1])).subtract(phi[l-1][14]);
	    		    p[l - 1] = yy[l - 1].add(tau);
	    		    phi[l-1][15] = (p[l - 1].subtract(yy[l - 1])).subtract(tau);
	    		} // for (l = 1; l <= neqn; l++)
	    	} // if (!nornd)
	    	else {
	    		for (l = 1; l <= neqn; l++) {
	    			p[l - 1] = yy[l - 1].add(h.multiply(p[l - 1]));
	    		} // for (l = 1; l <= neqn; l++)
	    	} // else
	    	xold = x;
	    	x = x.add(h);
	    	absh = h.abs();
		    if (testMode) {
		    	fTestMode(x, p, yp);
		    }
		    else {
		        f(x, p, yp);
		    }
		    //
		    // estimate errors at k,k-1,k-2
		    //
		    erkm2 = DoubleDouble.valueOf(0.0);
		    erkm1 = DoubleDouble.valueOf(0.0);
		    erk = DoubleDouble.valueOf(0.0);
		    for (l = 1; l <= neqn; l++) {
		    	temp3 = wt[l - 1].reciprocal();
		    	temp4 = yp[l - 1].subtract(phi[l-1][0]);
		    	if (km2 > 0) {
		    		erkm2 = erkm2.add(((phi[l-1][km1-1].add(temp4)).multiply(temp3)).pow(2));
		    	}
		    	if (km2 >= 0) {
		    		erkm1 = erkm1.add(((phi[l-1][k-1].add(temp4)).multiply(temp3)).pow(2));
		    	}
		        erk = erk.add((temp4.multiply(temp3)).pow(2));
		    } // for (l = 1; l <= neqn; l++)
		    if (km2 > 0) {
		    	erkm2 = ((absh.multiply(sig[km1 -1])).multiply(gstr[km2-1])).multiply(erkm2.sqrt());
		    }
		    if (km2 >= 0) {
		    	erkm1 = ((absh.multiply(sig[k - 1])).multiply(gstr[km1-1])).multiply(erkm1.sqrt());
		    }
		    temp5 = absh.multiply(erk.sqrt());
		    err = temp5.multiply(g[k - 1].subtract(g[kp1 - 1]));
		    erk = (temp5.multiply(sig[kp1 -1])).multiply(gstr[k - 1]);
		    knew = k;
		    //
		    // test if order should be lowered
		    //
		    if (km2 > 0) {
		    	if (erkm1.max(erkm2).le(erk)) {
		    		knew = km1;
		    	}
		    } // if (km2 > 0)
		    else if (km2 == 0) {
		    	if (erkm1.le((DoubleDouble.valueOf(0.5)).multiply(erk))) {
		    		knew = km1;
		    	}
		    } // else if (km2 == 0)
		    //
		    // test if successful
		    //
		    if (err.le(eps[0])) {
		    	break;
		    }
	    	//       ***     end block 2     ***
	    	//
	    	//       ***     begin block 3     ***
	    	//   the step is unsuccessful.  restore  x, phi(*,*), psi(*) .
	    	//   if third consecutive failure, set order to one.  if step fails more
	    	//   than three times, consider an optimal step size.  DoubleDouble error
	    	//   tolerance and return if estimated step size is too small for machine
	    	//   precision.
	    	//                   ***
	    	//
	    	//   restore x, phi(*,*) and psi(*)
	    	//
	    	phase1 = false;
	    	x = xold;
	    	for (i = 1; i <= k; i++) {
	    	    temp1 = beta[i - 1].reciprocal();
	    	    ip1 = i+1;
	    	    for (l = 1; l <= neqn; l++) {
	    	        phi[l-1][i-1] = temp1.multiply(phi[l-1][i-1].subtract(phi[l-1][ip1-1]));	
	    	    } // for (l = 1; l <= neqn; l++) 
	    	} // for (i = 1; i <= k; i++)
	    	if (k >= 2) {
	    		for (i = 2; i <= k; i++) {
	    		    psi[i - 2] = psi[i - 1].subtract(h);	
	    		} // for (i = 2; i <= k; i++)
	    	} // if (k >= 2)
	    	//
	    	// on third failure, set order to one.  thereafter, use optimal step size
	    	//
	    	ifail = ifail + 1;
	    	temp2 = DoubleDouble.valueOf(0.5);
	    	if ((ifail -3) > 0) {
	    		if (p5eps.lt((DoubleDouble.valueOf(0.25)).multiply(erk))) {
	    			temp2 = (p5eps.divide(erk)).sqrt();
	    		}
	    	} // if ((ifail -3) > 0)
	    	if ((ifail - 3) >= 0) {
	    		knew = 1;
	    	}
	    	h = temp2.multiply(h);
	    	k = knew;
		    if ((h.abs()).ge(fouru.multiply(x.abs()))) {
		    	continue;
		    }
		    crash[0] = true;
		    if (h.ge(DoubleDouble.valueOf(0.0))) {
		    	h = fouru.multiply(x.abs());
		    }
		    else {
		    	h = (fouru.multiply(x.abs())).negate();
		    }
		    eps[0] = eps[0].add(eps[0]);
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
	    temp1 = h.multiply(g[kp1 - 1]);
	    if (!nornd) {
	    	for (l = 1; l <= neqn; l++) {
	    	    rho = (temp1.multiply(yp[l-1].subtract(phi[l-1][0]))).subtract(phi[l-1][15]); 
	    	    yy[l-1] = p[l-1].add(rho);
	    	    phi[l-1][14] = (yy[l-1].subtract(p[l-1])).subtract(rho);
	    	} // for (l = 1; l <= neqn; l++)
	    } // if (!nornd)
	    else {
	    	for (l = 1; l <= neqn; l++) {
	    	    yy[l-1] = p[l-1].add(temp1.multiply(yp[l-1].subtract(phi[l-1][0])));	
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
	        phi[l-1][kp1-1] = yp[l-1].subtract(phi[l-1][0]);
	        phi[l-1][kp2-1] = phi[l-1][kp1-1].subtract(phi[l-1][kp2-1]);
	    } // for (l = 1; l <= neqn; l++)
	    for (i = 1; i <= k; i++) {
	        for (l = 1; l <= neqn; l++) { 
	        	phi[l-1][i-1] = phi[l-1][i-1].add(phi[l-1][kp1-1]);
	        } // for (l = 1; l <= neqn; l++) 
	    } // for (i = 1; i <= k; i++)
	    //
	    //   estimate error at order k+1 unless:
	    //     in first phase when always raise order,
	    //     already decided to lower order,
	    //     step size not constant so estimate unreliable
	    //
	    erkp1 = DoubleDouble.valueOf(0.0);
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
	            	    erkp1 = erkp1.add((phi[l-1][kp2-1].divide(wt[l-1])).pow(2));	
	            	} // for (l = 1; l <= neqn; l++)
	            	erkp1 = (absh.multiply(gstr[kp1-1])).multiply(erkp1.sqrt());
	            	//
	            	// using estimated error at order k+1, determine approximate order
	            	// for next step
	            	//
	            	if (k <= 1) {
	            	    if (erkp1.ge((DoubleDouble.valueOf(0.5)).multiply(erk))) {
	            	    	do450 = false;
	            	    	do455 = false;
	            	    }
	            	    else {
	            	    	do450 = true;
	            	    	do455 = true;
	            	    }
	            	} // if (k <= 1)
	            	else { // k > 1
	            	    if (erkm1.le(erk.min(erkp1))) {
	            	    	do450 = false;
	            	    	do455 = true;
	            	    }
	            	    else {
	            	    	if ((erkp1.ge(erk)) || (k == 12)) {
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
	    hnew = h.add(h);
	    if (phase1) {
	    	h = hnew;
	    	return;
	    }
	    if (p5eps.ge(erk.multiply(two[k]))) {
	    	h = hnew;
	    	return;
	    }
	    hnew = h;
	    if (p5eps.ge(erk)) {
	    	h = hnew;
	    	return;
	    }
	    temp2 = DoubleDouble.valueOf(k + 1);
	    r = (p5eps.divide(erk)).pow(temp2.reciprocal());
	    hnew = absh.multiply((DoubleDouble.valueOf(0.5)).max((DoubleDouble.valueOf(0.9)).min(r)));
	    if (h.ge(DoubleDouble.valueOf(0.0))) {
	    	hnew = (hnew.max(fouru.multiply(x.abs()))).abs();
	    }
	    else {
	    	hnew = ((hnew.max(fouru.multiply(x.abs()))).abs()).negate();
	    }
	    h = hnew;
	    return;
	    // ***  end block 4 ***
    }
    
    public abstract void f(DoubleDouble t, DoubleDouble y[], DoubleDouble yp[]);
    
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
    	//   all floating point variables are DoubleDouble precision
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
    	DoubleDouble psijm1;
    	DoubleDouble gamma;
    	DoubleDouble eta;
    	int limit1;
    	DoubleDouble temp2;
    	DoubleDouble temp3;
    	DoubleDouble g[] = new DoubleDouble[13];
    	DoubleDouble w[] = new DoubleDouble[13];
    	DoubleDouble rho[] = new DoubleDouble[13];
    	g[0] = DoubleDouble.valueOf(1.0);
    	rho[0] = DoubleDouble.valueOf(1.0);
    	
    	DoubleDouble hi = tout.subtract(x);
    	int ki = kold + 1;
    	int kip1 = ki + 1;
    	//
    	// initialize w[] for computing g[]
    	//
    	for (i = 1; i <= ki; i++) {
    		w[i-1] = (DoubleDouble.valueOf(i)).reciprocal();
    	}
    	DoubleDouble term = DoubleDouble.valueOf(0.0);
    	//
    	// compute g[]
    	//
    	for (j = 2; j <= ki; j++) {
    	    jm1 = j - 1;
    	    psijm1 = psi[jm1 - 1];
    	    gamma = (hi.add(term)).divide(psijm1);
    	    eta = hi.divide(psijm1);
    	    limit1 = kip1 - j;
    	    for (i = 0; i < limit1; i++) {
    	    	w[i] = (gamma.multiply(w[i])).subtract(eta.multiply(w[i+1]));
    	    }
    	    g[j-1] = w[0];
    	    rho[j-1] = gamma.multiply(rho[jm1-1]);
    	    term = psijm1;
    	} // for (j = 2; j <= ki; j++)
    	//
    	// interpolate
    	//
    	for (l = 0; l < neqn; l++) {
    	    ypout[l] = DoubleDouble.valueOf(0.0);
    		y[l] = DoubleDouble.valueOf(0.0);
    	} // for (l = 0; l < neqn; l++)
    	for (j = 1; j <= ki; j++) {
    	    i = kip1 - j;
    	    temp2 = g[i-1];
    	    temp3 = rho[i-1];
    	    for (l = 0; l < neqn; l++) {
    	        y[l] = y[l].add(temp2.multiply(phi[l][i-1]));
    	        ypout[l] = ypout[l].add(temp3.multiply(phi[l][i-1]));
    	    } // for (l = 0; l < neqn; l++)
    	} // for (j = 1; j <= ki; j++)
    	for (l = 0; l < neqn; l++) {
    	    y[l] = yy[l].add(hi.multiply(y[l]));	
    	} // for (l = 0; l < neqn; l++)
    	return;
    }
}