package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/** This is a port of the FORTRAN90 source code dqed.f90.  DQED solves (square) systems of nonlinear equations,
    or minimizes the residual in a set of nonlinear equations, using least squares.  The user may include 
    simple bounds or linear constraints on variables.  DQED was written by Richard Hanson and Fred Krogh
    of Sandia National Laboratory. */

public abstract class DQED {
	
	private int idope[] = new int[6];
	
	private int igo_dbocls = 0;
	
	private final double one = 1.0;
	
	private boolean outputMes = true;
	
	// epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.224460e-16
    // epsilon is called the largest relative spacing
    /*epsilon = 1.0;
    neweps = 1.0;

    while (true) {

        if (1.0 == (1.0 + neweps)) {
            break;
        } else {
            epsilon = neweps;
            neweps = neweps / 2.0;
        }
    } // while(true)*/
	private double epsilon = Math.pow(2.0, -52.0);
	
	private int igo_dbols = 0;
	
	private int iscale_dbols;
	
	private int locacc_dbols;
	
	private int lopt_dbols;
	
	
	public DQED() {
		
	}
	
	private double damax(int n, double x[], int incx) {
		/*****************************************************************************80
		!
		!! DAMAX returns the maximum absolute value of the entries in a vector.
		!
		!  Modified:
		!
		!    08 April 1999
		!
		!  Parameters:
		!
		!    Input, integer N, the number of entries in the vector.
		!
		!    Input, double X(*), the vector to be examined.
		!
		!    Input, integer INCX, the increment between successive entries of X.
		!
		!    Output, double DAMAX, the maximum absolute value of an 
		!    element of X.
		*/

		  int i;
		  int ix;
		  double result = 0.0;

		  if ( n <= 0 ) {
		    result = 0.0;
		  }
		  else if ( n == 1 ) {
		    result = Math.abs ( x[1] );
		  }
		  else if ( incx == 1 ) {
		    result = Math.abs ( x[1] );

		    for (i = 2; i <= n; i++) {
		      if ( Math.abs ( x[i] ) > result ) {
		        result = Math.abs ( x[i] );
		      }
		    }
		  }
		  else {
		    if ( incx >= 0 ) {
		      ix = 1;
		    }
		    else {
		      ix = ( - n + 1 ) * incx + 1;
		    }

		    result = Math.abs ( x[ix] );
		    ix = ix + incx;

		    for (i = 2; i <= n; i++) {
		      if ( Math.abs ( x[ix] ) > result ) {
		        result = Math.abs ( x[ix] );
		      }
		      ix = ix + incx;
		    }

		  }

		  return result;
	} // damax
	
	private double damsum(int n, double x[], int incx) {
		/*****************************************************************************80
		!
		!! DASUM sums the absolute values of the entries of a vector.
		!
		!  Modified:
		!
		!    08 April 1999
		!
		!  Author:
		!
		!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
		!
		!  Reference:
		!
		!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
		!    Basic Linear Algebra Subprograms for Fortran Usage,
		!    Algorithm 539,
		!    ACM Transactions on Mathematical Software,
		!    Volume 5, Number 3, September 1979, pages 308-323.
		!
		!  Parameters:
		!
		!    Input, integer N, the number of entries in the vector.
		!
		!    Input, double X(*), the vector to be examined.
		!
		!    Input, integer INCX, the increment between successive entries of X.
		!
		!    Output, double DASUM, the sum of the absolute values of 
		!    the vector.
		*/

		  int i;
		  int ix;
		  int m;
		  double result = 0.0;

		  if ( n <= 0 ) {
			  
		  }
		  else if ( incx == 1 ) {

		    m =  ( n  % 6 );

		    for (i = 1; i <=  m; i++) {
		      result = result + Math.abs ( x[i] );
		    }

		    for (i = m+1; i <= n; i += 6) {
		      result = result + Math.abs ( x[i]   ) + Math.abs ( x[i+1] ) + Math.abs ( x[i+2] )
		                    + Math.abs ( x[i+3] ) + Math.abs ( x[i+4] ) + Math.abs ( x[i+5] );
		    }
		  }
		  else {

		    if ( incx >= 0 ) {
		      ix = 1;
		    }
		    else {
		      ix = ( - n + 1 ) * incx + 1;
		    }

		    for (i = 1; i <= n; i++) {
		      result = result + Math.abs ( x[ix] );
		      ix = ix + incx;
		    }

		  }

		  return result;
	} // dasum
	
	private void daxpy(int n, double sa, double x[], int incx, double y[], int incy) {
		/*****************************************************************************80
		!
		!! DAXPY adds a constant times one vector to another.
		!
		!  Modified:
		!
		!    08 April 1999
		!
		!  Author:
		!
		!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
		!
		!  Reference:
		!
		!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
		!    Basic Linear Algebra Subprograms for Fortran Usage,
		!    Algorithm 539,
		!    ACM Transactions on Mathematical Software,
		!    Volume 5, Number 3, September 1979, pages 308-323.
		!
		!  Parameters:
		!
		!    Input, integer N, the number of entries in the vector.
		!
		!    Input, double SA, the multiplier.
		!
		!    Input, double X(*), the vector to be scaled and added to Y.
		!
		!    Input, integer INCX, the increment between successive entries of X.
		!
		!    Input/output, double Y(*), the vector to which a multiple 
		!    of X is to be added.
		!
		!    Input, integer INCY, the increment between successive entries of Y.
		*/

		  int i;
		  int ix;
		  int iy;

		  if ( n <= 0 ) {
			  
		  }
		  else if ( sa == 0.0 ) {
			  
		  }
		  else if ( incx == 1 && incy == 1 ) {
              for (i = 1; i <= n; i++) {
		          y[i] = y[i] + sa * x[i];
              }
		  }
		  else {

		    if ( incx >= 0 ) {
		      ix = 1;
		    }
		    else {
		      ix = ( - n + 1 ) * incx + 1;
		    }

		    if ( incy >= 0 ) {
		      iy = 1;
		    }
		    else {
		      iy = ( - n + 1 ) * incy + 1;
		    }

		    for (i = 1; i <= n; i++) {
		      y[iy] = y[iy] + sa * x[ix];
		      ix = ix + incx;
		      iy = iy + incy;
		    }
		  }

		  return;	
	} // daxpy
	
	private void dbocls ( double w[][], int mdw, int mcon, int mrows, int ncols, double bl[], double bu[],
			              int ind[], int iopt[], double x[], double rnormc[], double rnorm[], int mode[],
			              double rw[], int iw[] ) {

			/*****************************************************************************80
			!
			!! DBOCLS solves a bounded and constrained least squares problem.
			!
			!  Discussion:
			!
			!    DBOCLS solves the bounded and constrained least squares
			!    problem consisting of solving the equation
			!      E*X = F  (in the least squares sense)
			!    subject to the linear constraints
			!      C*X = Y.
			!
			!    This subprogram solves the bounded and constrained least squares
			!    problem. The problem statement is:
			!
			!    Solve E*X = F (least squares sense), subject to constraints
			!    C*X=Y.
			!
			!    In this formulation both X and Y are unknowns, and both may
			!    have bounds on any of their components.  This formulation
			!    of the problem allows the user to have equality and inequality
			!    constraints as well as simple bounds on the solution components.
			!
			!    This constrained linear least squares subprogram solves E*X=F
			!    subject to C*X=Y, where E is MROWS by NCOLS, C is MCON by NCOLS.
			!
			!    The user must have dimension statements of the form
			!
			!      real ( kind = 8 ) W(MDW,NCOLS+MCON+1)
			!      real ( kind = 8 ) BL(NCOLS+MCON)
			!      real ( kind = 8 ) BU(NCOLS+MCON),
			!      real ( kind = 8 ) X(2*(NCOLS+MCON)+2+NX)
			!      real ( kind = 8 ) RW(6*NCOLS+5*MCON)
			!      integer IND(NCOLS+MCON)
			!      integer IOPT(17+NI)
			!      integer IW(2*(NCOLS+MCON))
			!
			!    Here NX is the number of extra locations required for the options; NX=0
			!    if no options are in use. Also NI is the number of extra locations
			!    for options 1-9.
			!
			!  Author:
			!
			!    Richard Hanson,
			!    Sandia National Laboratory
			!
			!  Reference:
			!
			!    Richard Hanson,
			!    Linear Least Squares with Bounds and Linear Constraints,
			!    SIAM Journal on Scientific and Statistical Computing,
			!    Volume 7, Number 3, July 1986.
			!
			!    INPUT
			!    -----
			!
			!    -------------------------
			!    W(MDW,*),MCON,MROWS,NCOLS
			!    -------------------------
			!     The array W contains the (possibly null) matrix [C:*] followed by
			!     [E:F].  This must be placed in W as follows:
			!          [C  :  *]
			!     W  = [       ]
			!          [E  :  F]
			!     The (*) after C indicates that this data can be undefined. The
			!     matrix [E:F] has MROWS rows and NCOLS+1 columns. The matrix C is
			!     placed in the first MCON rows of W(*,*) while [E:F]
			!     follows in rows MCON+1 through MCON+MROWS of W(*,*). The vector F
			!     is placed in rows MCON+1 through MCON+MROWS, column NCOLS+1. The
			!     values of MDW and NCOLS must be positive; the value of MCON must
			!     be nonnegative. An exception to this occurs when using option 1
			!     for accumulation of blocks of equations. In that case MROWS is an
			!     OUTPUT variable only, and the matrix data for [E:F] is placed in
			!     W(*,*), one block of rows at a time. See IOPT(*) contents, option
			!     number 1, for further details. The row dimension, MDW, of the
			!     array W(*,*) must satisfy the inequality:
			!
			!     If using option 1,
			!                     MDW >= MCON + max(max. number of
			!                     rows accumulated, NCOLS)
			!
			!     If using option 8, MDW >= MCON + MROWS.
			!     Else, MDW >= MCON + max(MROWS, NCOLS).
			!
			!     Other values are errors, but this is checked only when using
			!     option=2.  The value of MROWS is an output parameter when
			!     using option number 1 for accumulating large blocks of least
			!     squares equations before solving the problem.
			!     See IOPT(*) contents for details about option 1.
			!
			!    ------------------
			!    BL(*),BU(*),IND(*)
			!    ------------------
			!     These arrays contain the information about the bounds that the
			!     solution values are to satisfy. The value of IND(J) tells the
			!     type of bound and BL(J) and BU(J) give the explicit values for
			!     the respective upper and lower bounds on the unknowns X and Y.
			!     The first NVARS entries of IND(*), BL(*) and BU(*) specify
			!     bounds on X; the next MCON entries specify bounds on Y.
			!
			!    1.    For IND(J)=1, require X(J) >= BL(J);
			!          IF J > NCOLS,        Y(J-NCOLS) >= BL(J).
			!          (the value of BU(J) is not used.)
			!    2.    For IND(J)=2, require X(J) <= BU(J);
			!          IF J > NCOLS,        Y(J-NCOLS) <= BU(J).
			!          (the value of BL(J) is not used.)
			!    3.    For IND(J)=3, require X(J) >= BL(J) and
			!                                X(J) <= BU(J);
			!          IF J > NCOLS,        Y(J-NCOLS) >= BL(J) and
			!                                Y(J-NCOLS) <= BU(J).
			!          (to impose equality constraints have BL(J)=BU(J)=
			!          constraining value.)
			!    4.    For IND(J)=4, no bounds on X(J) or Y(J-NCOLS) are required.
			!          (the values of BL(J) and BU(J) are not used.)
			!
			!     Values other than 1,2,3 or 4 for IND(J) are errors. In the case
			!     IND(J)=3 (upper and lower bounds) the condition BL(J)  >  BU(J)
			!     is  an  error.   The values BL(J), BU(J), J  >  NCOLS, will be
			!     changed.  Significant changes mean that the constraints are
			!     infeasible.  (Users must make this decision themselves.)
			!     The new values for BL(J), BU(J), J  >  NCOLS, define a
			!     region such that the perturbed problem is feasible.  If users
			!     know that their problem is feasible, this step can be skipped
			!     by using option number 8 described below.
			!
			!    -------
			!    IOPT(*)
			!    -------
			!     This is the array where the user can specify nonstandard options
			!     for DBOCLS( ). Most of the time this feature can be ignored by
			!     setting the input value IOPT(1)=99. Occasionally users may have
			!     needs that require use of the following subprogram options. For
			!     details about how to use the options see below: IOPT(*) CONTENTS.
			!
			!     Option Number   Brief Statement of Purpose
			!     ------ ------   ----- --------- -- -------
			!           1         Return to user for accumulation of blocks
			!                     of least squares equations.  The values
			!                     of IOPT(*) are changed with this option.
			!                     The changes are updates to pointers for
			!                     placing the rows of equations into position
			!                     for processing.
			!           2         Check lengths of all arrays used in the
			!                     subprogram.
			!           3         Column scaling of the data matrix, [C].
			!                                                        [E]
			!           4         User provides column scaling for matrix [C].
			!                                                             [E]
			!           5         Provide option array to the low-level
			!                     subprogram DBOLS( ).
			!                     {Provide option array to the low-level
			!                     subprogram DBOLSM( ) by imbedding an
			!                     option array within the option array to
			!                     DBOLS(). Option 6 is now disabled.}
			!           7         Move the IOPT(*) processing pointer.
			!           8         Do not preprocess the constraints to
			!                     resolve infeasibilities.
			!           9         Do not pretriangularize the least squares matrix.
			!          99         No more options to change.
			!
			!    ----
			!    X(*)
			!    ----
			!     This array is used to pass data associated with options 4,5 and
			!     6. Ignore this parameter (on input) if no options are used.
			!     Otherwise see below: IOPT(*) CONTENTS.
			!
			!
			!    OUTPUT
			!    ------
			!
			!    -----------------
			!    X(*),RNORMC,RNORM
			!    -----------------
			!     The array X(*) contains a solution (if MODE >=0 or  == -22) for
			!     the constrained least squares problem. The value RNORMC is the
			!     minimum residual vector length for the constraints C*X - Y = 0.
			!     The value RNORM is the minimum residual vector length for the
			!     least squares equations. Normally RNORMC=0, but in the case of
			!     inconsistent constraints this value will be nonzero.
			!     The values of X are returned in the first NVARS entries of X(*).
			!     The values of Y are returned in the last MCON entries of X(*).
			!
			!    ----
			!    MODE
			!    ----
			!     The sign of MODE determines whether the subprogram has completed
			!     normally, or encountered an error condition or abnormal status. A
			!     value of MODE >= 0 signifies that the subprogram has completed
			!     normally. The value of mode (>= 0) is the number of variables
			!     in an active status: not at a bound nor at the value zero, for
			!     the case of free variables. A negative value of MODE will be one
			!     of the cases (-57)-(-41), (-37)-(-22), (-19)-(-2). Values  <  -1
			!     correspond to an abnormal completion of the subprogram. These
			!     error messages are in groups for the subprograms DBOCLS(),
			!     DBOLSM(), and DBOLS().  An approximate solution will be returned
			!     to the user only when max. iterations is reached, MODE=-22.
			!
			!    -----------
			!    RW(*),IW(*)
			!    -----------
			!     These are working arrays.  (normally the user can ignore the
			!     contents of these arrays.)
			!
			!    IOPT(*) CONTENTS
			!    ------- --------
			!     The option array allows a user to modify some internal variables
			!     in the subprogram without recompiling the source code. A central
			!     goal of the initial software design was to do a good job for most
			!     people. Thus the use of options will be restricted to a select
			!     group of users. The processing of the option array proceeds as
			!     follows: a pointer, here called LP, is initially set to the value
			!     1. At the pointer position the option number is extracted and
			!     used for locating other information that allows for options to be
			!     changed. The portion of the array IOPT(*) that is used for each
			!     option is fixed; the user and the subprogram both know how many
			!     locations are needed for each option. The value of LP is updated
			!     for each option based on the amount of storage in IOPT(*) that is
			!     required. A great deal of error checking is done by the
			!     subprogram on the contents of the option array. Nevertheless it
			!     is still possible to give the subprogram optional input that is
			!     meaningless. For example option 4 uses the locations
			!     X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1) for passing scaling data.
			!     The user must manage the allocation of these locations.
			!
			!   1
			!   -
			!     This option allows the user to solve problems with a large number
			!     of rows compared to the number of variables. The idea is that the
			!     subprogram returns to the user (perhaps many times) and receives
			!     new least squares equations from the calling program unit.
			!     Eventually the user signals "that's all" and a solution is then
			!     computed. The value of MROWS is an output variable when this
			!     option is used. Its value is always in the range 0 <= MROWS
			!     <= NCOLS+1. It is the number of rows after the
			!     triangularization of the entire set of equations. If LP is the
			!     processing pointer for IOPT(*), the usage for the sequential
			!     processing of blocks of equations is
			!
			!
			!        IOPT(LP)=1
			!         Move block of equations to W(*,*) starting at
			!         the first row of W(*,*).
			!        IOPT(LP+3)=# of rows in the block; user defined
			!
			!     The user now calls DBOCLS( ) in a loop. The value of IOPT(LP+1)
			!     directs the user's action. The value of IOPT(LP+2) points to
			!     where the subsequent rows are to be placed in W(*,*). Both of
			!     these values are first defined in the subprogram. The user
			!     changes the value of IOPT(LP+1) (to 2) as a signal that all of
			!     the rows have been processed.
			!
			!
			!      .<LOOP
			!      . CALL DBOCLS( )
			!      . IF(IOPT(LP+1) .EQ. 1) THEN
			!      .    IOPT(LP+3)=# OF ROWS IN THE NEW BLOCK; USER DEFINED
			!      .    PLACE NEW BLOCK OF IOPT(LP+3) ROWS IN
			!      .    W(*,*) STARTING AT ROW MCON + IOPT(LP+2).
			!      .
			!      .    IF( THIS IS THE LAST BLOCK OF EQUATIONS ) THEN
			!      .       IOPT(LP+1)=2
			!      .<------CYCLE LOOP
			!      .    ELSE IF (IOPT(LP+1) .EQ. 2) THEN
			!      <-------EXIT LOOP SOLUTION COMPUTED IF MODE .GE. 0
			!      . ELSE
			!      . ERROR CONDITION; SHOULD NOT HAPPEN.
			!      .<END LOOP
			!
			!     Use of this option adds 4 to the required length of IOPT(*).
			!
			!   2
			!   -
			!     This option is useful for checking the lengths of all arrays used
			!     by DBOCLS( ) against their actual requirements for this problem.
			!     The idea is simple: the user's program unit passes the declared
			!     dimension information of the arrays. These values are compared
			!     against the problem-dependent needs within the subprogram. If any
			!     of the dimensions are too small an error message is printed and a
			!     negative value of MODE is returned, -41 to -47. The printed error
			!     message tells how long the dimension should be. If LP is the
			!     processing pointer for IOPT(*),
			!
			!        IOPT(LP)=2
			!        IOPT(LP+1)=Row dimension of W(*,*)
			!        IOPT(LP+2)=Col. dimension of W(*,*)
			!        IOPT(LP+3)=Dimensions of BL(*),BU(*),IND(*)
			!        IOPT(LP+4)=Dimension of X(*)
			!        IOPT(LP+5)=Dimension of RW(*)
			!        IOPT(LP+6)=Dimension of IW(*)
			!        IOPT(LP+7)=Dimension of IOPT(*)
			!         .
			!        CALL DBOCLS( )
			!
			!     Use of this option adds 8 to the required length of IOPT(*).
			!
			!   3
			!   -
			!     This option can change the type of scaling for the data matrix.
			!     Nominally each nonzero column of the matrix is scaled so that the
			!     magnitude of its largest entry is equal to the value ONE. If LP
			!     is the processing pointer for IOPT(*),
			!
			!        IOPT(LP)=3
			!        IOPT(LP+1)=1,2 or 3
			!            1= Nominal scaling as noted;
			!            2= Each nonzero column scaled to have length ONE;
			!            3= Identity scaling; scaling effectively suppressed.
			!         .
			!        CALL DBOCLS( )
			!
			!     Use of this option adds 2 to the required length of IOPT(*).
			!
			!   4
			!   -
			!     This options allows the user to provide arbitrary (positive)
			!     column scaling for the matrix. If LP is the processing pointer
			!     for IOPT(*),
			!
			!        IOPT(LP)=4
			!        IOPT(LP+1)=IOFF
			!        X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1)
			!        = Positive scale factors for cols. of E.
			!         .
			!        CALL DBOCLS( )
			!
			!     Use of this option adds 2 to the required length of IOPT(*)
			!     and NCOLS to the required length of X(*).
			!
			!   5
			!   -
			!     This option allows the user to provide an option array to the
			!     low-level subprogram DBOLS( ). If LP is the processing pointer
			!     for IOPT(*),
			!
			!        IOPT(LP)=5
			!        IOPT(LP+1)= Position in IOPT(*) where option array
			!                    data for DBOLS( ) begins.
			!         .
			!        CALL DBOCLS( )
			!
			!     Use of this option adds 2 to the required length of IOPT(*).
			!
			!   6
			!   -
			!     This option is no longer operative.  To pass an option array
			!     to the low-level subprogram DBOLSM( ), imbed it within an option
			!     array passed to DBOLS() using option 5.
			!
			!   7
			!   -
			!     Move the processing pointer (either forward or backward) to the
			!     location IOPT(LP+1). The processing pointer moves to locations
			!     LP+2 if option number 7 is used with the value -7.  For
			!     example to skip over locations 3,...,NCOLS+2,
			!
			!       IOPT(1)=7
			!       IOPT(2)=NCOLS+3
			!       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
			!       IOPT(NCOLS+3)=99
			!       CALL DBOCLS( )
			!
			!     CAUTION: Misuse of this option can yield some very hard-to-find
			!     bugs. Use it with care. It is intended to be used for passing
			!     option arrays to other subprograms.
			!
			!   8
			!   -
			!     This option allows the user to suppress the algorithmic feature
			!     of DBOCLS( ) that processes the constraint equations C*X = Y and
			!     resolves infeasibilities. The steps normally done are to solve
			!     C*X - Y = 0 in a least squares sense using the stated bounds on
			!     both X and Y. Then the "reachable" vector Y = C*X is computed
			!     using the solution X obtained. Finally the stated bounds for Y are
			!     enlarged to include C*X. To suppress the feature:
			!
			!
			!       IOPT(LP)=8
			!         .
			!       CALL DBOCLS( )
			!
			!     Use of this option adds 1 to the required length of IOPT(*).
			!
			!   9
			!   -
			!     This option allows the user to suppress the pretriangularizing
			!     step of the least squares matrix that is done within DBOCLS( ).
			!     This is primarily a means of enhancing the subprogram efficiency
			!     and has little effect on accuracy. To suppress the step, set:
			!
			!       IOPT(LP)=9
			!         .
			!       CALL DBOCLS( )
			!
			!     Use of this option adds 1 to the required length of IOPT(*).
			!
			!   99
			!   --
			!     There are no more options to change.
			!
			!     Only option numbers -99, -9,-8,...,-1, 1,2,...,9, and 99 are
			!     permitted. Other values are errors. Options -99,-1,...,-9 mean
			!     that the respective options 99,1,...,9 are left at their default
			!     values. An example is the option to suppress the preprocessing of
			!     constraints:
			!
			!       IOPT(1)=-8 Option is recognized but not changed
			!       IOPT(2)=99
			!       CALL DBOCLS( )
			*/

			  boolean accum = false;
			  double anorm;
			  boolean checkl;
			  double cnorm;
			  double drelpr = epsilon;
			  boolean filter = false;
			  int i;
			  int icase;
			  int idum;
			  int iiw;
			  int inrows;
			  int ip;
			  int irw;
			  int iscale = 0;
			  int j;
			  int jopt[] = new int[6];
			  int jp;
			  int lds;
			  int lenx;
			  int level;
			  int liopt = 0;
			  int liw;
			  int llb = 0;
			  int lliw = 0;
			  int llrw = 0;
			  int llx = 0;
			  int lmdw = 0;
			  int lndw = 0;
			  int locacc = 0;
			  int locdim;
			  int lopt = 0;
			  int lp;
			  int lrw;
			  int mdwl;
			  int mnew = 0;
			  int modec[] = new int[1];;
			  int mout = 0;
			  int nerr;
			  boolean pretri = false;
			  double rdum;
			  double t = 0.0;
			  double t1;
			  double t2;
			  double wt;
			  double arr[];
			  double rwArr[];
			  int ioptArr[];
			  int iwArr[];
			  double wArr[][];

			  idum = 0;
			  rdum = 0.0;
			  nerr = 0;
			  mode[0] = 0;
			  level = 1;

			  if ( igo_dbocls == 0) {
			//
			//  Check validity of input data.
			//
			//  See that mdw is .gt.0. gross check only.
			//
			      if ( mdw<=0) {
			          nerr = 53;
			          xerrwv("dbocls(). mdw=(i1) must be positive.",
			                      nerr,level,1,mdw,idum,0,rdum,rdum);
				      if ( 0 <= mode[0] ) {
					    mode[0] = -nerr;
				      }
	
					  igo_dbocls = 0;

				      return;
			      } // if (mdw <= 0)
			//
			//  See that number of constraints is nonnegative.
			//
			      if ( mcon < 0) {
			          nerr = 54;
			          xerrwv("dbocls(). mcon=(i1) must be nonnegative.",
			                      nerr,level,1,mcon,idum,0,rdum,rdum);
				      if ( 0 <= mode[0] ) {
				          mode[0] = -nerr;
					  }
			
				      igo_dbocls = 0;
			
					  return;
			      } // if (mcon < 0)
			//
			//  See that number of unknowns is positive.
			//
			      if ( ncols<=0) {
			          nerr = 55;
			          xerrwv("dbocls(). ncols=(i1) the no. of variables must be positive.",
			                      nerr,level,1,ncols,idum,0,rdum,rdum);
			          if ( 0 <= mode[0] ) {
			              mode[0] = -nerr;
		              }

			          igo_dbocls = 0;

		              return; 
			     } // if (ncols <= 0)
			//
			//  See that constraint indicators are all well-defined.
			//
			      for (j = 1; j <= ncols + mcon; j++) {
			          if ( ind[j] < 1 || ind[j] > 4) {
			              nerr = 56;
			              xerrwv("dbocls(). for j=(i1), ind(j)=(i2) must be 1-4.",
			                          nerr,level,2,j,ind[j],0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
					          mode[0] = -nerr;
				          }
	
					      igo_dbocls = 0;

				          return;
			          } // if ( ind[j] < 1 || ind[j] > 4)
			      } // for (j = 1; j <= ncols + mcon; j++)
			//
			//  See that bounds are consistent.
			//
			      for (j = 1; j <= ncols + mcon; j++) {
			          if ( ind[j] == 3) {
			              if ( bl[j] > bu[j]) {
			                  nerr = 57;
			                  xerrwv("dbocls(). for j=(i1), bound bl(j)=(r1) is  >  bu(j)=(r2).",
			                         nerr,level,1,j,idum,2,bl[j],bu[j]);
			                  if ( 0 <= mode[0] ) {
					              mode[0] = -nerr;
				              }
	
					          igo_dbocls = 0;

				              return;
			              } // if (bl[j] > bu[j])
	                  } // if (ind[j] == 3)
	              } // for (j = 1; j <= ncols + mcon; j++)
			//
			//  Process option array.
			//
			      
			        
			      drelpr = epsilon;
			      checkl = false;
			      filter = true;
			      lenx = 2* (ncols+mcon) + 2;
			      iscale = 1;
			      igo_dbocls = 1;
			      accum = false;
			      pretri = true;
			      lopt = 0;
			      lp = 0;
			      lds = 0;

		loop:     while (true) {
			      lp = lp + lds;
			      ip = iopt[lp+1];
			      jp = Math.abs(ip);
			//
			//  Test for no more options to change.
			//
			      if ( ip == 99) {
			          if ( lopt == 0) {
			        	  lopt = lp+1;
			          }
			//
			// Send column scaling to DBOLS().
			//
			          idope[4]=1;
			//
			//  Note that DBOLS() was called by DBOCLS()
			//
			          idope[5]=1;
			//
			//  Change pretriangularization factor in DBOLSM().
			//
			          idope[1] = ncols + mcon + 1;
			//
			//  Pass weight to DBOLSM() for rank test.
			//
			          idope[2] = ncols + mcon + 2;
			          idope[3] = mcon;
			          break loop;
			      } // if (ip == 99)
			      else if ( jp == 99) {
			          lds = 1;
			          break loop;
			      } // else if (jp == 99)
			      else if ( jp == 1) {
			          if ( ip > 0) {
			//
			//  Set up direction flag location, row stacking pointer
			//  location, and location for number of new rows.
			//
			              locacc = lp + 2;
			//
			//                  IOPT(LOCACC-1)=option number for seq. accumulation.
			//     Contents..   IOPT(LOCACC  )=user direction flag, 1 or 2.
			//                  IOPT(LOCACC+1)=row stacking pointer.
			//                  IOPT(LOCACC+2)=number of new rows to process.
			//
			//     User action with this option..
			//
			//      (Set up option data for SEQ. Accumulation in IOPT(*).)
			//      (Move block of equations into W(*,*)  starting at first
			//       row of W(*,*) below the rows for the constraint matrix C.
			//       Set IOPT(LOCACC+2)=no. of least squares equations in block.
			//
			//              LOOP
			//              CALL DBOCLS()
			//
			//                  IF(IOPT(LOCACC) .EQ. 1) THEN
			//                      STACK EQUAS. INTO W(*,*), STARTING AT
			//                      ROW IOPT(LOCACC+1).
			//                       INTO W(*,*).
			//                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
			//                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
			//                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
			//                      (PROCESS IS OVER. EXIT LOOP.)
			//                  ELSE
			//                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
			//                  END IF
			//              END LOOP
			//
			              iopt[locacc+1] = mcon + 1;
			              accum = true;
			              iopt[locacc] = igo_dbocls;
			          } // if (ip > 0)
			          lds = 4;
			          continue loop;
			      } // else if (jp == 1)
			      else if ( jp == 2) {
			          if ( ip > 0) {
			//
			//  GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
			//
			              locdim = lp + 2;
			//
			//  LMDW.GE.MCON+MAX(MOUT,NCOLS), IF MCON.GT.0 .AND FILTER
			//  LMDW.GE.MCON+MOUT, OTHERWISE
			
			//  LNDW.GE.NCOLS+MCON+1
			//  LLB .GE.NCOLS+MCON
			//  LLX .GE.2*(NCOLS+MCON)+2+EXTRA REQD. IN OPTIONS.
			//  LLRW.GE.6*NCOLS+5*MCON
			//  LLIW.GE.2*(NCOLS+MCON)
			//  LIOP.GE. AMOUNT REQD. FOR OPTION ARRAY.
			//
			              lmdw = iopt[locdim];
			              lndw = iopt[locdim+1];
			              llb = iopt[locdim+2];
			              llx = iopt[locdim+3];
			              llrw = iopt[locdim+4];
			              lliw = iopt[locdim+5];
			              liopt = iopt[locdim+6];
			              checkl = true;
			          } // if (ip > 0)
			          lds = 8;
			          continue loop;
			      } // else if (jp == 2)
			//
			//  OPTION TO MODIFY THE COLUMN SCALING.
			//
			     
			      else if ( jp == 3) {
			          if ( ip > 0) {
			              iscale = iopt[lp+2];
			//
			//     SEE THAT ISCALE IS 1 THRU 3.
			//
			              if ( iscale < 1 || iscale > 3) {
			                  nerr = 48;
			                  xerrwv("dbocls(). iscale option=(i1) must be 1-3.",
			                    nerr,level,1,iscale,idum,0,rdum,rdum);
			                  if ( 0 <= mode[0] ) {
					              mode[0] = -nerr;
				              }
	
					          igo_dbocls = 0;

				              return;
			              } // if ( iscale < 1 || iscale > 3)
			          } // if (ip > 0)
			          lds = 2;
			          continue loop;
			      } // else if (jp == 3)
			//
			//  IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
			//  SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
			//
			      else if ( jp == 4) {
			          if ( ip > 0) {
			              iscale = 4;
			              if ( iopt[lp+2]<=0) {
			                  nerr = 49;
			                  xerrwv("dbocls(). offset past x(ncols) (i1) for\nuser-provided column scaling must be positive.",
			                    nerr,level,1,iopt[lp+2],idum,0,rdum,rdum);
			                  if ( 0 <= mode[0] ) {
					              mode[0] = -nerr;
				              }
	
					          igo_dbocls = 0;

				              return;
			              } // if ( iopt[lp+2]<=0)
			              for (i = 1; i <= ncols; i++) {
			            	  rw[i] = x[ncols + iopt[lp+2] + i - 1];
			              }
			              lenx = lenx + ncols;
			              for (j = 1; j <= ncols; j++) {
			                  if ( rw[j] <= 0.0 ) {
			                      nerr = 50;
			                      xerrwv("dbocls(). each provided col. scale\nfactor must be positive. comp. (i1)   now = (r1).",
			                        nerr,level,1,j,idum,1,rw[j],rdum);
			                      if ( 0 <= mode[0] ) {
							          mode[0] = -nerr;
						          }
			
							      igo_dbocls = 0;

						          return;
			                  } // if (rw[j] <= 0.0)
			              } // for (j = 1; j <= ncols; j++)
			          } // if (ip > 0)
			          lds = 2;
			          continue loop;
			      } // else if (jp == 4)
			//
			//  IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLS().
			//
			      else if ( jp == 5) {
			          if ( ip > 0) {
			              lopt = iopt[lp+2];
			          } // if (ip > 0)
			          lds = 2;
			          continue loop;
			      } // else if (jp == 5)
			//
			//  IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLSM().
			//  (NO LONGER USED.) OPTION NOW MUST BE PASSED IMBEDDED IN
			//  OPTION ARRAY FOR DBOLS().
			//
			      else if ( jp == 6) {
			          lds = 2;
			          continue loop;
			      }
			//
			//  THIS OPTION USES THE NEXT LOC OF IOPT(*) AS A
			//  POINTER VALUE TO SKIP TO NEXT.
			//
			      else if ( jp == 7) {
			          if ( ip > 0) {
			              lp = iopt[lp+2]-1;
			              lds = 0;
			          }
			          else {
			              lds = 2;
			          }
			          continue loop;
			      } // else if (jp == 7)
			//
			//  THIS OPTION AVOIDS THE CONSTRAINT RESOLVING PHASE FOR
			//  THE LINEAR CONSTRAINTS C*X=Y.
			//
			      else if ( jp == 8) {
			          filter = ! (ip > 0);
			          lds = 1;
			          continue loop;
			      } // else if (jp = 8)
			//
			//  THIS OPTION SUPPRESSES PRETRIANGULARIZATION OF THE LEAST
			//  SQUARES EQUATIONS.
			//
			      else if ( jp == 9) {
			          pretri = ! (ip > 0);
			          lds = 1;
			          continue loop;
			      } // else if (jp == 9)
			//
			//  NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
			//
			      else {
			          nerr = 51;
			          xerrwv("dbocls(). the option number=(i1) is not defined.",
			            nerr,level,1,jp,idum,0,rdum,rdum);
			          if ( 0 <= mode[0] ) {
				          mode[0] = -nerr;
				      }
			
					  igo_dbocls = 0;

					  return;
			      } // else 
		} // loop: while (do)

			      if ( checkl) {
			//
			//  CHECK LENGTHS OF ARRAYS
			//
			//  THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
			//  ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
			//
			       if ( filter && (!accum)) {
			         mdwl=mcon+Math.max(mrows,ncols);
			       }
			       else if ( accum) {
			         mdwl=mcon+ncols+1;
			       }
			       else {
			         mdwl=mcon+ncols;
			       }

			          if ( lmdw < mdwl) {
			              nerr = 41;
			              xerrwv("dbocls(). the row dimension of w(,)=(i1)\nmust be >= the number of effective rows=(i2).",
			                nerr,level,2,lmdw,mdwl,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
					           mode[0] = -nerr;
				          }
	
					      igo_dbocls = 0;

				          return;
			          } // if (lmdw < mdwl)
			          if ( lndw < ncols+mcon+1) {
			              nerr = 42;
			              xerrwv("dbocls(). the column dimension of w(,)=(i1)\nmust be >= ncols+mcon+1=(i2).",
			              nerr,level,2,lndw,ncols+mcon+1,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
						      mode[0] = -nerr;
					      }
		
						  igo_dbocls = 0;

					      return;
			          } // if ( lndw < ncols+mcon+1)
			          if ( llb < ncols+mcon) {
			              nerr = 43;
			              xerrwv("dbocls(). the dimensions of the arrays bl()\nbu(), and ind()=(i1) must be >= ncols+mcon=(i2).",
			                nerr,level,2,llb,ncols+mcon,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
						      mode[0] = -nerr;
					      }
		
						  igo_dbocls = 0;

					      return;
			          } // if ( llb < ncols+mcon)

			          if ( llx < lenx) {
			              nerr = 44;
			              xerrwv("dbocls(). the dimension of x()=(i1) must be\n>= the reqd. length=(i2).",
			            		  nerr,level,2,llx,lenx,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
						      mode[0] = -nerr;
					      }
		
						  igo_dbocls = 0;

					      return;
			          } // if (llx < lenx)

			          if ( llrw < 6*ncols+5*mcon) {
			              nerr = 45;
			              xerrwv("dbocls(). the dimension of rw()=(i1) must be\n>= 6*ncols+5*mcon=(i2).",
			            		  nerr,level,2,llrw,6*ncols+5*mcon,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
						      mode[0] = -nerr;
					      }
		
						  igo_dbocls = 0;

					      return;
			          } // if ( llrw < 6*ncols+5*mcon)

			          if ( lliw < 2*ncols+2*mcon) {
			              nerr = 46;
			              xerrwv("dbocls() the dimension of iw()=(i1) must be\n>= 2*ncols+2*mcon=(i2).",
			            		  nerr,level,2,lliw, 2*ncols+2*mcon,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
						      mode[0] = -nerr;
					      }
		
						  igo_dbocls = 0;

					      return;
			          } // if ( lliw < 2*ncols+2*mcon)

			          if ( liopt < lp+17) {
			              nerr = 47;
			              xerrwv("dbocls(). the dimension of iopt()=(i1) must be\n>= the reqd. len.=(i2).",
			            		  nerr,level,2,liopt,lp+17, 0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
						    mode[0] = -nerr;
					      }
		
						  igo_dbocls = 0;

					      return;
			          } // if ( liopt < lp+17)
			      } // if (checkl)
			  } // if (igo_dbocls == 0)
			//
			//  Optionally go back to the user for accumulation of least squares
			//  equations and directions for processing these equations.
			//
			//  Accumulate least squares equations.
			//
			  if ( accum) {
			      mrows = iopt[locacc+1] - 1 - mcon;
			      inrows = iopt[locacc+2];
			      mnew = mrows + inrows;
			      if ( mnew < 0 || mnew+mcon > mdw) {
			          nerr = 52;
			          xerrwv("dbocls(). no. of rows=(i1) must be >= 0\n.and. <=mdw-mcon=(i2)",
			        		  nerr,level,2,mnew,mdw-mcon,0,rdum,rdum);
			          if ( 0 <= mode[0] ) {
			              mode[0] = -nerr;
		              }

			          igo_dbocls = 0;

		              return;
			      } // if ( mnew < 0 || mnew+mcon > mdw)
			  } // if (accum)
			//
			// USE THE SOFTWARE OF DBOLS( ) FOR THE TRIANGULARIZATION OF THE
			//  LEAST SQUARES MATRIX.  THIS MAY INVOLVE A SYSTALTIC INTERCHANGE
			// OF PROCESSING POINTERS BETWEEN THE CALLING AND CALLED (DBOLS())
			// PROGRAM UNITS.
			//
			  jopt[1] = 1;
			  jopt[2] = 2;
			  jopt[4] = mrows;
			  jopt[5] = 99;
			  irw = ncols + 1;
			  iiw = 1;
			  if ( accum || pretri) {
			//
			//  NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
			//
			          idope[5]=0;
			          rwArr = new double[5*ncols + 1];
					  for (i = irw; i < rw.length && i < irw + 5*ncols; i++) {
						  rwArr[i - irw + 1] = rw[i];
					  }
					  iwArr = new int[2*ncols + 1];
					  for (i = iiw; i < iw.length && i < iiw + 2*ncols; i++) {
						  iwArr[i - iiw + 1] = iw[i];
					  }
					  wArr = new double[mdw][ncols+1];
					  for (i = 1; i <= mdw; i++) {
						  for (j = 1; j <= ncols+1; j++) {
							  wArr[i][j] = w[mcon+i][j];
						  }
					  }
			          dbols(wArr,mdw,mout,ncols,bl,bu,ind,jopt,x,rnorm,
			                mode,rwArr,iwArr);
					  for (i = 1; i <= mdw; i++) {
						  for (j = 1; j <= ncols+1; j++) {
							  w[mcon+i][j] = wArr[i][j];
						  }
					  }
					  for (i = irw; i < rw.length && i < irw + 5*ncols; i++) {
						  rw[i] = rwArr[i - irw + 1];
					  }
					  for (i = iiw; i < iw.length && i < iiw + 2*ncols; i++) {
						  iw[i] = iwArr[i - iiw + 1];
					  }
			  }
			  else {
			      mout = mrows;
			  }

			  if ( accum) {
			    accum = (iopt[locacc]  ==  1);
			    iopt[locacc+1] = jopt[3] + mcon;
			    mrows = Math.min(ncols+1,mnew);
			  }

			  if ( accum) {
				  return;
			  }
			//
			//  SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM
			//
			// MOVE RIGHT HAND SIDE OF LEAST SQUARES EQUATIONS.
			//
			  for (i = 1; i <= mout; i++) {
				  w[mcon+i][ncols+mcon+1] = w[mcon+i][ncols+1];
			  }
			  if ( mcon > 0 && filter) {
			//
			//  PROJECT THE LINEAR CONSTRAINTS INTO A REACHABLE SET.
			//
			      for (i = 1; i <= mcon; i++) {
			    	for (j = 1; j <= ncols; j++) {
			    	    w[mcon+j][ncols+i] = w[i][j];
			    	}
			      } // for (i = 1; i <= mcon; i++)
			//
			//  PLACE (-)IDENTITY MATRIX AFTER CONSTRAINT DATA.
			//
			      for (j = ncols + 1; j <= ncols + mcon + 1; j++) {
			    	for (i = 1; i <= mcon; i++) {
			            w[i][j] = 0.0;
			    	}
			      }
                  for (i = 1; i <= mcon; i++) {
			          w[i][ncols+1] = -1.0;
                  }
			//
			//  OBTAIN A 'FEASIBLE POINT' FOR THE LINEAR CONSTRAINTS.
			//
			      jopt[1] = 99;
			      irw = ncols + 1;
			      iiw = 1;
			//
			//  NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
			//
			          idope[5]=0;
			          rwArr = new double[5*(ncols+mcon) + 1];
					  for (i = irw; i < rw.length && i < irw + 5*(ncols + mcon); i++) {
						  rwArr[i - irw + 1] = rw[i];
					  }
					  iwArr = new int[2*(ncols+mcon) + 1];
					  for (i = iiw; i < iw.length && i < iiw + 2*(ncols + mcon); i++) {
						  iwArr[i - iiw + 1] = iw[i];
					  }
			          dbols(w,mdw,mcon,ncols+mcon,bl,bu,ind,jopt,x,rnormc,
			                modec,rwArr,iwArr);
					  for (i = irw; i < rw.length && i < irw + 5*(ncols + mcon); i++) {
						  rw[i] = rwArr[i - irw + 1];
					  }
					  for (i = iiw; i < iw.length && i < iiw + 2*(ncols + mcon); i++) {
						  iw[i] = iwArr[i - iiw + 1];
					  }
			//
			//  ENLARGE THE BOUNDS SET, IF REQUIRED, TO INCLUDE POINTS THAT
			//  CAN BE REACHED.
			//
			     for (j = ncols + 1; j <= ncols + mcon; j++) {
			          icase = ind[j];
			          if ( icase < 4) {
			        	  t = 0.0;
			        	  for (i = 1; i <= ncols; i++) {
			        		  t += w[mcon+i][j]*x[i];
			        	  }
			          } // if (icase < 4)
			          switch(icase) {
			              case 1:
			                  bl[j] = Math.min(t,bl[j]);
			                  break;
			              case 2:
			                  bu[j] = Math.max(t,bu[j]);
			                  break;
			              case 3:
			                  bl[j] = Math.min(t,bl[j]);
			                  bu[j] = Math.max(t,bu[j]);
			          } // switch(icase)
			     } // for (j = ncols + 1; j <= ncols + mcon; j++)
			//
			//  MOVE CONSTRAINT DATA BACK TO THE ORIGINAL AREA.
			//
			      for (j = ncols + 1; j <= ncols + mcon; j++) {
			    	  for (i = 1; i <= ncols; i++) {
			    		  w[j-ncols][i] = w[mcon+i][j];
			    	  }
			      } // for (j = ncols + 1; j <= ncols + mcon; j++)

			  } // if ( mcon > 0 && filter)

			  if ( mcon > 0) {
			      for (j = ncols + 1; j <= ncols + mcon; j++) {
			    	for (i = 1; i <= mout; i++) {
			          w[mcon+i][j] = 0.0;
			    	}
			      } // for (j = ncols + 1; j <= ncols + mcon; j++)
			//
			//  PUT IN (-)IDENTITY MATRIX (POSSIBLY) ONCE AGAIN.
		    //
			      for (j = ncols + 1; j <= ncols + mcon + 1; j++) {
			    	for (i = 1; i <= mcon; i++) {
			            w[i][j] = 0.0;
			    	}
			      } // for (j = ncols + 1; j <= ncols + mcon + 1; j++)
                  for (i = 1; i <= mcon; i++) {
			          w[i][ncols+1] = -1.0;
                  }

			  } // if (mcon > 0)
			//
			//  COMPUTE NOMINAL COLUMN SCALING FOR THE UNWEIGHTED MATRIX.
			//
			  cnorm = 0.0;
			  anorm = 0.0;
			  for (j = 1; j <= ncols; j++) {
				  t1 = 0.0;
				  for (i = 1; i <= mcon; i++) {
					  t1 += Math.abs(w[i][j]);
				  }
			      t2 = 0.0;
			      for (i = 1; i <= mout; i++) {
			    	  t2 += Math.abs(w[mcon+i][1]);
			      }
			      t = t1 + t2;
			      if ( t == 0.0 ) {
			    	  t = 1.0;
			      }
			      cnorm = Math.max(cnorm,t1);
			      anorm = Math.max(anorm,t2);
			      x[ncols+mcon+j] = 1.0/t;
			  } // for (j = 1; j <= ncols; j++)

			  switch (iscale) {
			  case 2:
			//
			//  SCALE COLS. (BEFORE WEIGHTING) TO HAVE LENGTH ONE.
			//

			    arr = new double[mcon+mout+1];
				for (j = 1; j <= ncols; j++) {
				    for (i = 1; i <= mcon+mout; i++) {
				    	arr[i] = w[i][j];
				    }
				    t = dnrm2(mcon+mout,arr,1);
				    if ( t == 0.0 ) {
				    	t = 1.0;
				    }
				    x[ncols+mcon+j] = 1.0/t;
			  } // for (j = 1; j <= ncols; j++)
			  break;
			//
			//  SUPPRESS SCALING (USE UNIT MATRIX).
			//
			  case 3:
                  for (i = 1; i <= ncols; i++) {
			          x[ncols+mcon+i] = 1.0;
                  }
                  break;
			//
			//  THE USER HAS PROVIDED SCALING.
			//
			  case 4:
				  for (i = 1; i <= ncols; i++) {
					  x[ncols+mcon+i] = rw[i];
				  }
			  } // switch (iscale)

			  for (j = ncols + 1; j <= ncols + mcon; j++) {
			    x[ncols+mcon+j] = 1.0;
			  }
			//
			//  WEIGHT THE LEAST SQUARES EQUATIONS.
			//
			  wt = Math.sqrt(drelpr);
			  if ( anorm > 0.0 ) {
				  wt = wt/anorm;
			  }
			  if ( cnorm > 0.0) {
				  wt = wt*cnorm;
			  }

			  for (i = 1; i <= mout; i++) {
				  for (j = 1; j <= ncols; j++) {
					  w[i+mcon][j] = wt * w[i+mcon][j];
				  }
			  } // for (i = 1; i <= mout; i++)
			  for (i = 1; i <= mout; i++) {
				  w[mcon+i][mcon+ncols+1] = wt*w[mcon+i][mcon+ncols+1];
			  }
			  lrw = 1;
			  liw = 1;
			//
			//  SET THE NEW TRIANGULARIZATION FACTOR.
			//
			  x[ncols+mcon+idope[1]]= 0.0;
			//
			//  SET THE WEIGHT TO USE IN COMPONENTS .GT. MCON,
			//  WHEN MAKING LINEAR INDEPENDENCE TEST.
			//
			  x[ncols+mcon+idope[2]] = 1.0/wt;
			  idope[5] = 1;
			  ioptArr = new int[iopt.length + 1 - lopt];
			  for (i = lopt; i < iopt.length; i++) {
				  ioptArr[i - lopt + 1] = iopt[i];
			  }
			  rwArr = new double[5*(ncols+mcon) + 1];
			  for (i = lrw; i < rw.length && i < lrw + 5*(ncols+mcon); i++) {
				  rwArr[i - lrw + 1] = rw[i];
			  }
			  iwArr = new int[2*(ncols+mcon) + 1];
			  for (i = liw; i < iw.length && i < liw + 2*(ncols+mcon); i++) {
				  iwArr[i - liw + 1] = iw[i];
			  }
			  dbols(w,mdw,mout+mcon,ncols+mcon,bl,bu,ind,ioptArr,x,
			        rnorm,mode,rwArr,iwArr);
			  for (i = lopt; i < iopt.length; i++) {
				  iopt[i] = ioptArr[i - lopt + 1];
			  }
			  for (i = lrw; i < rw.length && i < lrw + 5*(ncols+mcon); i++) {
				  rw[i] = rwArr[i - lrw + 1];
			  }
			  for (i = liw; i < iw.length && i < liw + 2*(ncols+mcon); i++) {
				  iw[i] = iwArr[i - liw + 1];
			  }

			  if ( 0 <= mode[0] ) {
			    mode[0] = -nerr;
			  }

			  igo_dbocls = 0;

			  return;
    } // dbocls
	
	private void dbols ( double w[][], int mdw, int mrows, int ncols, double bl[], double bu[],
			int ind[], int iopt[], double x[], double rnorm[], int mode[], double rw[], int iw[] ) {

			/*****************************************************************************80
			!
			!! DBOLS solves the linear system E*X = F in the least squares sense.
			!
			!  Discussion:
			!
			!    This routine solves the problem
			!
			!      E*X = F 
			!
			!    in the least squares sense with bounds on selected X values.
			!
			!  Modified:
			!
			!    02 September 2007
			!
			!  Author:
			!
			!    Richard Hanson,
			!    Sandia National Laboratory
			!
			!  Reference:
			!
			!    Richard Hanson,
			!    Linear Least Squares with Bounds and Linear Constraints,
			!    SIAM Journal on Scientific and Statistical Computing,
			!    Volume 7, Number 3, July 1986.
			!
			!***DESCRIPTION
			!
			!     The user must have dimension statements of the form:
			!
			!       DIMENSION W(MDW,NCOLS+1), BL(NCOLS), BU(NCOLS),
			!      * X(NCOLS+NX), RW(5*NCOLS)
			!       integer IND(NCOLS), IOPT(1+NI), IW(2*NCOLS)
			!
			!     (here NX=number of extra locations required for option 4; NX=0
			!     for no options; NX=NCOLS if this option is in use. Here NI=number
			!     of extra locations required for options 1-6; NI=0 for no
			!     options.)
			!
			!   INPUT
			!   -----
			!
			!    --------------------
			!    W(MDW,*),MROWS,NCOLS
			!    --------------------
			!     The array W(*,*) contains the matrix [E:F] on entry. The matrix
			!     [E:F] has MROWS rows and NCOLS+1 columns. This data is placed in
			!     the array W(*,*) with E occupying the first NCOLS columns and the
			!     right side vector F in column NCOLS+1. The row dimension, MDW, of
			!     the array W(*,*) must satisfy the inequality MDW >= MROWS.
			!     Other values of MDW are errrors. The values of MROWS and NCOLS
			!     must be positive. Other values are errors. There is an exception
			!     to this when using option 1 for accumulation of blocks of
			!     equations. In that case MROWS is an OUTPUT variable ONLY, and the
			!     matrix data for [E:F] is placed in W(*,*), one block of rows at a
			!     time.  MROWS contains the number of rows in the matrix after
			!     triangularizing several blocks of equations. This is an OUTPUT
			!     parameter ONLY when option 1 is used. See IOPT(*) CONTENTS
			!     for details about option 1.
			!
			!    ------------------
			!    BL(*),BU(*),IND(*)
			!    ------------------
			!     These arrays contain the information about the bounds that the
			!     solution values are to satisfy. The value of IND(J) tells the
			!     type of bound and BL(J) and BU(J) give the explicit values for
			!     the respective upper and lower bounds.
			!
			!    1.    For IND(J)=1, require X(J) >= BL(J).
			!          (the value of BU(J) is not used.)
			!    2.    For IND(J)=2, require X(J) <= BU(J).
			!          (the value of BL(J) is not used.)
			!    3.    For IND(J)=3, require X(J) >= BL(J) and
			!                                X(J) <= BU(J).
			!    4.    For IND(J)=4, no bounds on X(J) are required.
			!          (the values of BL(J) and BU(J) are not used.)
			!
			!     Values other than 1,2,3 or 4 for IND(J) are errors. In the case
			!     IND(J)=3 (upper and lower bounds) the condition BL(J)  >  BU(J)
			!     is an error.
			!
			!    -------
			!    IOPT(*)
			!    -------
			!     This is the array where the user can specify nonstandard options
			!     for DBOLSM( ). Most of the time this feature can be ignored by
			!     setting the input value IOPT(1)=99. Occasionally users may have
			!     needs that require use of the following subprogram options. For
			!     details about how to use the options see below: IOPT(*) CONTENTS.
			!
			!     Option Number   Brief Statement of Purpose
			!     ------ ------   ----- --------- -- -------
			!           1         Return to user for accumulation of blocks
			!                     of least squares equations.
			!           2         Check lengths of all arrays used in the
			!                     subprogram.
			!           3         Standard scaling of the data matrix, E.
			!           4         User provides column scaling for matrix E.
			!           5         Provide option array to the low-level
			!                     subprogram DBOLSM( ).
			!           6         Move the IOPT(*) processing pointer.
			!           7         User has called DBOLS() directly.
			!          99         No more options to change.
			!
			!    ----
			!    X(*)
			!    ----
			!     This array is used to pass data associated with option 4. Ignore
			!     this parameter if this option is not used. Otherwise see below:
			!     IOPT(*) CONTENTS.
			!
			!    OUTPUT
			!    ------
			!
			!    ----------
			!    X(*),RNORM
			!    ----------
			!     The array X(*) contains a solution (if MODE >=0 or  == -22) for
			!     the constrained least squares problem. The value RNORM is the
			!     minimum residual vector length.
			!
			!    ----
			!    MODE
			!    ----
			!     The sign of MODE determines whether the subprogram has completed
			!     normally, or encountered an error condition or abnormal status. A
			!     value of MODE >= 0 signifies that the subprogram has completed
			!     normally. The value of MODE (.GE. 0) is the number of variables
			!     in an active status: not at a bound nor at the value ZERO, for
			!     the case of free variables. A negative value of MODE will be one
			!     of the cases -37,-36,...,-22, or -17,...,-2. Values  <  -1
			!     correspond to an abnormal completion of the subprogram. To
			!     understand the abnormal completion codes see below: ERROR
			!     MESSAGES for DBOLS( ). AN approximate solution will be returned
			!     to the user only when max. iterations is reached, MODE=-22.
			!     Values for MODE=-37,...,-22 come from the low-level subprogram
			!     DBOLSM(). See the section ERROR MESSAGES for DBOLSM() in the
			!     documentation for DBOLSM().
			!
			!    -----------
			!    RW(*),IW(*)
			!    -----------
			!     These are working arrays with 5*NCOLS and 2*NCOLS entries.
			!     (normally the user can ignore the contents of these arrays,
			!     but they must be dimensioned properly.)
			!
			!    IOPT(*) CONTENTS
			!    ------- --------
			!     The option array allows a user to modify internal variables in
			!     the subprogram without recompiling the source code. A central
			!     goal of the initial software design was to do a good job for most
			!     people. Thus the use of options will be restricted to a select
			!     group of users. The processing of the option array proceeds as
			!     follows: a pointer, here called LP, is initially set to the value
			!     1. This value is updated as each option is processed. At the
			!     pointer position the option number is extracted and used for
			!     locating other information that allows for options to be changed.
			!     The portion of the array IOPT(*) that is used for each option is
			!     fixed; the user and the subprogram both know how many locations
			!     are needed for each option. A great deal of error checking is
			!     done by the subprogram on the contents of the option array.
			!     Nevertheless it is still possible to give the subprogram optional
			!     input that is meaningless. For example option 4 uses the
			!     locations X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1) for passing
			!     scaling data. The user must manage the allocation of these
			!     locations.
			!
			!   1
			!   -
			!     This option allows the user to solve problems with a large number
			!     of rows compared to the number of variables. The idea is that the
			!     subprogram returns to the user (perhaps many times) and receives
			!     new least squares equations from the calling program unit.
			!     Eventually the user signals "that's all" and then computes the
			!     solution with one final call to subprogram DBOLS( ). The value of
			!     MROWS is an OUTPUT variable when this option is used. Its value
			!     is always in the range 0 <= MROWS .le. NCOLS+1. It is equal to
			!     the number of rows after the triangularization of the entire set
			!     of equations. If LP is the processing pointer for IOPT(*), the
			!     usage for the sequential processing of blocks of equations is
			!
			!        IOPT(LP)=1
			!        Move block of equations to W(*,*) starting at
			!        the first row of W(*,*).
			!        IOPT(LP+3)=# of rows in the block; user defined
			!
			!     The user now calls DBOLS( ) in a loop. The value of IOPT(LP+1)
			!     directs the user's action. The value of IOPT(LP+2) points to
			!     where the subsequent rows are to be placed in W(*,*).
			!
			!      .<LOOP
			!      . CALL DBOLS()
			!      . IF(IOPT(LP+1) .EQ. 1) THEN
			!      .    IOPT(LP+3)=# OF ROWS IN THE NEW BLOCK; USER DEFINED
			!      .    PLACE NEW BLOCK OF IOPT(LP+3) ROWS IN
			!      .    W(*,*) STARTING AT ROW IOPT(LP+2).
			!      .
			!      .    IF( THIS IS THE LAST BLOCK OF EQUATIONS ) THEN
			!      .       IOPT(LP+1)=2
			!      .<------CYCLE LOOP
			!      .    ELSE IF (IOPT(LP+1) .EQ. 2) THEN
			!      <-------EXIT LOOP SOLUTION COMPUTED IF MODE .GE. 0
			!      . ELSE
			!      . ERROR CONDITION; SHOULD NOT HAPPEN.
			!      .<END LOOP
			!
			!     Use of this option adds 4 to the required length of IOPT(*).
			!
			!
			!   2
			!   -
			!     This option is useful for checking the lengths of all arrays used
			!     by DBOLS() against their actual requirements for this problem.
			!     The idea is simple: the user's program unit passes the declared
			!     dimension information of the arrays. These values are compared
			!     against the problem-dependent needs within the subprogram. If any
			!     of the dimensions are too small an error message is printed and a
			!     negative value of MODE is returned, -11 to -17. The printed error
			!     message tells how long the dimension should be. If LP is the
			!     processing pointer for IOPT(*),
			!
			!        IOPT(LP)=2
			!        IOPT(LP+1)=Row dimension of W(*,*)
			!        IOPT(LP+2)=Col. dimension of W(*,*)
			!        IOPT(LP+3)=Dimensions of BL(*),BU(*),IND(*)
			!        IOPT(LP+4)=Dimension of X(*)
			!        IOPT(LP+5)=Dimension of RW(*)
			!        IOPT(LP+6)=Dimension of IW(*)
			!        IOPT(LP+7)=Dimension of IOPT(*)
			!         .
			!        CALL DBOLS()
			!
			!     Use of this option adds 8 to the required length of IOPT(*).
			!
			!   3
			!   -
			!     This option changes the type of scaling for the data matrix E.
			!     Nominally each nonzero column of E is scaled so that the
			!     magnitude of its largest entry is equal to the value ONE. If LP
			!     is the processing pointer for IOPT(*),
			!
			!        IOPT(LP)=3
			!        IOPT(LP+1)=1,2 or 3
			!            1= Nominal scaling as noted;
			!            2= Each nonzero column scaled to have length ONE;
			!            3= Identity scaling; scaling effectively suppressed.
			!         .
			!        CALL DBOLS()
			!
			!     Use of this option adds 2 to the required length of IOPT(*).
			!
			!   4
			!   -
			!     This option allows the user to provide arbitrary (positive)
			!     column scaling for the matrix E. If LP is the processing pointer
			!     for IOPT(*),
			!
			!        IOPT(LP)=4
			!        IOPT(LP+1)=IOFF
			!        X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1)
			!        = Positive scale factors for cols. of E.
			!         .
			!        CALL DBOLS()
			!
			!     Use of this option adds 2 to the required length of IOPT(*) and
			!     NCOLS to the required length of X(*).
			!
			!   5
			!   -
			!     This option allows the user to provide an option array to the
			!     low-level subprogram DBOLSM(). If LP is the processing pointer
			!     for IOPT(*),
			!
			!        IOPT(LP)=5
			!        IOPT(LP+1)= Position in IOPT(*) where option array
			!                    data for DBOLSM() begins.
			!         .
			!        CALL DBOLS()
			!
			!     Use of this option adds 2 to the required length of IOPT(*).
			!
			!   6
			!   -
			!     Move the processing pointer (either forward or backward) to the
			!     location IOPT(LP+1). The processing point is moved to entry
			!     LP+2 of IOPT(*) if the option is left with -6 in IOPT(LP).  For
			!     example to skip over locations 3,...,NCOLS+2 of IOPT(*),
			!
			!       IOPT(1)=6
			!       IOPT(2)=NCOLS+3
			!       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
			!       IOPT(NCOLS+3)=99
			!       CALL DBOLS()
			!
			!     CAUTION: Misuse of this option can yield some very hard
			!     -to-find bugs.  Use it with care.
			!
			!   7
			!   -
			!     If the user is calling DBOLS() directly, use this option.
			!     (This is necessary because DBOCLS() uses DBOLS() as a
			!     low-level subprogram.  Due to weighting required within
			!     DBOCLS(), the two cases must be known.) For example,
			!
			!       IOPT(1)=7
			!       IOPT(1)=99
			!
			!   99
			!   --
			!     There are no more options to change.
			!
			!     Only option numbers -99, -7,-6,-5,...,-1, 1,2,...,7, and 99 are
			!     permitted. Other values are errors. Options -99,-1,...,-7 mean
			!     that the repective options 99,1,...,7 are left at their default
			!     values. An example is the option to modify the (rank) tolerance:
			!
			!       IOPT(1)=-3 Option is recognized but not changed
			!       IOPT(2)=2  Scale nonzero cols. to have length ONE
			!       IOPT(3)=99
			!
			!***ROUTINES CALLED  IDAMAX,DBOLSM,DCOPY,DNRM2,DROT,DROTG,XERRWV
			!***COMMON BLOCKS    DBOCOM
			!***END PROLOGUE  DBOLS
			!
			!     SOLVE LINEAR LEAST SQUARES SYSTEM WITH BOUNDS ON
			!     SELECTED VARIABLES.
			*/

			  //real ( kind = 8 ) bl(ncols)
			  //real ( kind = 8 ) bu(ncols)
			  boolean checkl;
			  int i;
			  int ibig;
			  int idum;
			  //integer ind(ncols)
			  int inrows;
			  int ip;
			  //integer iw(2*ncols)
			  int j;
			  int jp;
			  int lds;
			  int lenx;
			  int level;
			  int liopt = 0;
			  int llb = 0;
			  int lliw = 0;
			  int llrw = 0;
			  int llx = 0;
			  int lmdw = 0;
			  int lndw = 0;
			  int locdim;
			  int lp;
			  int mnew;
			  int nerr;
			  double rdum;
			  //real ( kind = 8 ) rw(5*ncols)
			  double sa[] = new double[1];
			  double sb[] = new double[1];
			  double sc[] = new double[1];
			  double ss[] = new double[1];
			  //real ( kind = 8 ) w(mdw,ncols+1)
			  double arr[];
			  double arr2[];
			  double blArr[];
			  double buArr[];
			  int ioptArr[];
			  double rwArr[];
			  double wwArr[];
			  int ibbArr[];
			  int k;

			  idum = 0;
			  rdum = 0.0;
			  nerr = 0;
			  mode[0] = 0;
			  level = 1;

			  if ( igo_dbols == 0 ) {
			//
			//  CHECK VALIDITY OF INPUT DATA
			//
			//  SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
			//
			      if ( mdw<=0) {
			          nerr = 2;
			          xerrwv("dbols(). mdw=(i1) must be positive.",
			            nerr,level,1,mdw,idum,0,rdum,rdum);
			          if ( 0 <= mode[0] ) {
			              mode[0] = -nerr;
			          }

			          igo_dbols = 0;
			          return;
			      } // if (mdw <= 0)
			//
			//  SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
			//
			      if ( ncols<=0) {
			          nerr = 3;
			          xerrwv("dbols(). ncols=(i1) the no. of variables must be positive.",
			                  nerr,level,1,ncols,idum,0,rdum,rdum);
			          if ( 0 <= mode[0] ) {
		                  mode[0] = -nerr;
		              }

		              igo_dbols = 0;
		              return;
			      } // if (ncols <= 0)
			//
			//  SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
			//
			      for (j = 1; j <= ncols; j++) {
			          if ( ind[j] < 1 || ind[j] > 4) {
			              nerr = 4;
			              xerrwv("dbols(). for j=(i1), ind(j)=(i2) must be 1-4.",
			                      nerr,level,2,j,ind[j],0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
		                      mode[0] = -nerr;
		                  }

		                  igo_dbols = 0;
		                  return;
			          } // if (ind[j] < 1 || ind[j] > 4)
			      } // for (j = 1; j <= ncols; j++)
			//
			//  SEE THAT BOUNDS ARE CONSISTENT.
			//
			      for (j = 1; j <= ncols; j++) {
			          if ( ind[j] == 3) {
			              if ( bl[j] > bu[j]) {
			                  nerr = 5;
			                  xerrwv("dbols(). for j=(i1), bound bl(j)=(r1) is  >  bu(j)=(r2).",
			                         nerr,level,1,j,idum,2,bl[j],bu[j]);
			                  if ( 0 <= mode[0] ) {
		                          mode[0] = -nerr;
		                      }

		                      igo_dbols = 0;
		                      return;
			              } // if (bl[j] > bu[j])
			          } // if (ind[j] == 3)
			      } // for (j = 1; j <= ncols; j++)
			//
			//  PROCESS OPTION ARRAY
			//
			      checkl = false;
			      lenx = ncols;
			      iscale_dbols = idope[4];
			      igo_dbols = 2;
			      lopt_dbols = 0;
			      lp = 0;
			      lds = 0;

			loop: while (true) {

			      lp = lp + lds;
			      ip = iopt[lp+1];
			      jp = Math.abs(ip);
			//
			//  TEST FOR NO MORE OPTIONS.
			//
			      if ( ip == 99) {
			          if ( lopt_dbols == 0) {
			            lopt_dbols = lp + 1;
			          } // if (lopt == 0)
			          break loop;
			      } // if (ip == 99)
			      else if ( jp == 99) {
			          lds = 1;
			          continue loop;
			      } // else if (jp == 99)
			      else if ( jp == 1) {
			          if ( ip > 0) {
			//
			//  SET UP DIRECTION FLAG, ROW STACKING POINTER
			//  LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
			//
			              locacc_dbols = lp + 2;
			//
			//                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
			//     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
			//                  IOPT(LOCACC+1)=ROW STACKING POINTER.
			//                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
			//     USER ACTION WITH THIS OPTION..
			//      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).
			//      MUST ALSO START PROCESS WITH IOPT(LOCACC)=1.)
			//      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
			//       ROW OF W(*,*).  SET IOPT(LOCACC+2)=NO. OF ROWS IN BLOCK.)
			//             LOOP
			//              CALL DBOLS()
			//
			//                 IF(IOPT(LOCACC) .EQ. 1) THEN
			//                      STACK EQUAS., STARTING AT ROW IOPT(LOCACC+1),
			//                       INTO W(*,*).
			//                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
			//                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
			//                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
			//                      (PROCESS IS OVER. EXIT LOOP.)
			//                  ELSE
			//                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
			//                  END IF
			//              END LOOP
			//              SET IOPT(LOCACC-1)=-OPTION NUMBER FOR SEQ. ACCUMULATION.
			//              CALL DBOLS( )
			              iopt[locacc_dbols+1] = 1;
			              igo_dbols = 1;
			          } // if (ip > 0)
			          lds = 4;
			          continue loop;
			      } // else if (jp == 1)
			      else if ( jp == 2) {
			          if ( ip > 0) {
			//
			//  GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
			//
			              locdim = lp + 2;
			//
			//  LMDW.GE.MROWS
			//  LNDW.GE.NCOLS+1
			//  LLB .GE.NCOLS
			//  LLX .GE.NCOLS+EXTRA REQD. IN OPTIONS.
			//  LLRW.GE.5*NCOLS
			//  LLIW.GE.2*NCOLS
			//  LIOP.GE. AMOUNT REQD. FOR IOPTION ARRAY.
			//
			              lmdw = iopt[locdim];
			              lndw = iopt[locdim+1];
			              llb = iopt[locdim+2];
			              llx = iopt[locdim+3];
			              llrw = iopt[locdim+4];
			              lliw = iopt[locdim+5];
			              liopt = iopt[locdim+6];
			              checkl = true;
			          } // if (ip > 0)
			          lds = 8;
			          continue loop;
			      } // else if (jp == 2)
			//
			//  OPTION TO MODIFY THE COLUMN SCALING.
			//
			      else if ( jp == 3) {
			          if ( ip > 0) {
			              iscale_dbols = iopt[lp+2];
			//
			//  SEE THAT ISCALE IS 1 THRU 3.
			//
			              if ( iscale_dbols < 1 || 3 < iscale_dbols ) {
			                  nerr = 7;
			                  xerrwv("dbols(). iscale option=(i1) must be 1-3.",
			                          nerr,level,1,iscale_dbols,idum,0,rdum,rdum);
			                  if ( 0 <= mode[0] ) {
	                              mode[0] = -nerr;
	                          }

	                          igo_dbols = 0;
	                          return;
			              } // if ( iscale < 1 || 3 < iscale )
			          } // if (ip > 0)
			          lds = 2;
			          continue loop;
			      } // else if (jp == 3)
			//
			//  IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
			//  SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
			//
			      else if ( jp == 4) {
			          if ( ip > 0) {
			              iscale_dbols = 4;
			              if ( iopt[lp+2]<=0) {
			                  nerr = 8;
			                  xerrwv("dbols(). offset past x(ncols) (i1)\nfor user-provided column scaling must be positive.",
			                         nerr,level,1,iopt[lp+2],idum,0,rdum,rdum);
			                  if ( 0 <= mode[0] ) {
				                  mode[0] = -nerr;
				              }

				              igo_dbols = 0;
				              return;
			              } // if (iopt[lp+2] <= 0)
			              for (i = 1; i <= ncols; i++) {
			            	  rw[i] = x[ncols + iopt[lp+2] + i - 1];
			              }
			              lenx = lenx + ncols;
			              for (j = 1; j <= ncols; j++) {
			                  if ( rw[j]<= 0.0 ) {
			                      nerr = 9;
			                      xerrwv("dbols(). each provided col. scale factor\nmust be positive. component (i1) now = (r1).",
			                              nerr,level,1,j,idum,1,rw[j],rdum);
			                      if ( 0 <= mode[0] ) {
				                      mode[0] = -nerr;
				                  }

				                  igo_dbols = 0;
				                  return;
			                  } // if (rw[j] <= 0.0)
			              } // for (j = 1; j <= ncols; j++)
			          } // if (ip > 0)
			          lds = 2;
			          continue loop;
			      } // else if (jp == 4)
			//
			//  IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLSM().
			//
			      else if ( jp == 5) {
			          if ( ip > 0) {
			              lopt_dbols = iopt[lp+2];
			          } // if (ip > 0)
			          lds = 2;
			          continue loop;
			      } // else if (jp == 5)
			//
			//  THIS OPTION USES THE NEXT LOC OF IOPT(*) AS THE PLACE TO
			//  MOVE AT THE NEXT STEP OF PROCESSESING.
			//
			      else if ( jp == 6) {
			          if ( ip > 0) {
			              lp = iopt[lp+2]-1;
			              lds = 0;
			          }
			          else {
			              lds = 2;
			          }
			          continue loop;
			      } // else if (jp == 6)
			//
			//  THIS OPTION PROVIDES INFORMATION ABOUT WHO CALLED DBOLS.
			//  IT WAS EITHER DBOCLS() OR THE USER.

			      else if ( jp == 7) {
			          lds=1;
			          if ( ip > 0) {
			            idope[5]=0;
			            iscale_dbols=1;
			          } // if (ip > 0)
			          continue loop;
			      } // else if (jp == 7)
			//
			//  NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
			//
			      else {
			          nerr = 6;
			          xerrwv("dbols(). the option number=(i1) is not defined.",
			                  nerr,level,1,jp,idum,0,rdum,rdum);
			          if ( 0 <= mode[0] ) {
				          mode[0] = -nerr;
				      }

				      igo_dbols = 0;
				      return;
			      }
			} // loop: while (true)

			      if ( checkl) {
			//
			//  CHECK LENGTHS OF ARRAYS
			//
			//  THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
			//  ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
			//
			          if ( lmdw < mrows) {
			              nerr = 11;
			              xerrwv("dbols(). the row dimension of w(,)=(i1)\nmust be >=the number of rows=(i2).",
			            		  nerr,level,2,lmdw,mrows,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
		                      mode[0] = -nerr;
		                  }

		                  igo_dbols = 0;
		                  return;
			          } // if (lmdw < mrows)
			          if ( lndw < ncols+1) {
			              nerr = 12;
			              xerrwv("dbols(). the column dimension of w(,)=(i1)\nmust be >= ncols+1=(i2).",
			            		  nerr,level,2,lndw,ncols+1,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
			          } // if (lndw < ncols + 1)
			          if ( llb < ncols) {
			              nerr = 13;
			              xerrwv("dbols(). the dimensions of the arrays bl(),bu(),\nand ind()=(i1) must be >= ncols=(i2).",
			                      nerr,level,2,llb,ncols,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
			          } // if (llb < ncols)
			          if ( llx < lenx) {
			              nerr = 14;
			              xerrwv("dbols(). the dimension of x()=(i1) must be\n>= the reqd. length=(i2).",
			            		  nerr,level,2,llx,lenx,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
			          } // if (llx < lenx)
			          if ( llrw < 5*ncols) {
			              nerr = 15;
			              xerrwv("dbols(). the dimension of rw()=(i1) must be >= 5*ncols=(i2).",
			            		  nerr,level,2,llrw,5*ncols,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
			          } // if (llrw < 5*ncols)
			          if ( lliw < 2*ncols) {
			              nerr = 16;
			              xerrwv("dbols() the dimension of iw()=(i1) must be >= 2*ncols=(i2).",
			            		  nerr,level,2,lliw,2*ncols,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
			          } // if (lliw < 2*ncols)
			          if ( liopt < lp+1) {
			              nerr = 17;
			              xerrwv("dbols(). the dimension of iopt()=(i1) must be\n>= the reqd. len.=(i2).",
			            		  nerr,level,2,liopt,lp+1,0,rdum,rdum);
			              if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
			          } // if (liopt < 2*ncols)

			      } // if (checkl)
			  } // if (igo_dbols == 0)

			  if ( igo_dbols == 1 ) {
				  //
	              //  GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES
			      //  EQUATIONS AND DIRECTIONS TO QUIT PROCESSING.
				  //
				  //
				  //  ACCUMULATE LEAST SQUARES EQUATIONS
				  //
					  mrows = iopt[locacc_dbols+1] - 1;
					  inrows = iopt[locacc_dbols+2];
					  mnew = mrows + inrows;

					  if ( mnew < 0 || mnew > mdw) {
					      nerr = 10;
					      xerrwv("dbols(). no. of rows=(i1) must be >= 0 .and. <= mdw=(i2).",
					              nerr,level,2,mnew,mdw,0,rdum,rdum);
					      if ( 0 <= mode[0] ) {
			                  mode[0] = -nerr;
			              }

			              igo_dbols = 0;
			              return;
					  } // if ( mnew < 0 || mnew > mdw)

					  for (j = 1; j <= Math.min(ncols+1,mnew); j++) {
					      for (i = mnew; i >= Math.max(mrows,j) + 1; i--) {
					    	  arr = new double[i-j+1];
					    	  for (k = 1; k <= i-j; k++) {
					    		  arr[k] = w[j+k][j];
					    	  }
					          ibig = idamax(i-j,arr,1) + j - 1;
					//
					//  PIVOT FOR INCREASED STABILITY.
					//
					          sa[0] = w[ibig][j];
					          sb[0] = w[i][j];
					          drotg(sa,sb,sc,ss);
					          w[ibig][j] = sa[0];
					          w[i][j] = sb[0];
					          arr = new double[ncols+2-j];
					          arr2 = new double[ncols+2-j];
					          for (k = 1; k <= ncols+1-j; k++) {
					        	  arr[k] = w[ibig][j+k];
					        	  arr2[k] = w[i][j+k];
					          }
					          drot(ncols+1-j,arr,1,arr2,1,sc[0],ss[0]);
					          for (k = 1; k <= ncols+1-j; k++) {
					        	  w[ibig][j+k] = arr[k];
					        	  w[i][j+k] = arr2[k];
					          }
					          w[i][j] = 0.0;
					      } // for (i = mnew; i >= Math.max(mrows,j) + 1, i--)
					  } // for (j = 1; j <= Math.min(ncols+1,mnew); j++)

					  mrows = Math.min(ncols+1,mnew);
					  iopt[locacc_dbols+1] = mrows + 1;
					  igo_dbols = iopt[locacc_dbols];

					  if ( igo_dbols == 2) {
					      igo_dbols = 0;
					  } // if (igo_dbols)
					  return;
			  } // if (igo_dbols == 1)
			  else if ( igo_dbols == 2 ) {
		          //
				  //  INITIALIZE VARIABLES AND DATA VALUES
				  //
				  for (j = 1; j <= ncols; j++) {
					//
					//  THIS IS THE NOMINAL SCALING. EACH NONZERO
					//  COL. HAS MAX. NORM EQUAL TO ONE.
					//
					    if ( iscale_dbols == 1 ) {
                          arr = new double[mrows+1];
                          for (k = 1; k <= mrows; k++) {
                        	  arr[k] = w[k][j];
                          }
					      ibig = idamax(mrows,arr,1);
					      rw[j] = Math.abs(w[ibig][j]);
					      if ( rw[j] == 0.0) {
					          rw[j] = 1.0;
					      }
					      else {
					          rw[j] = 1.0/rw[j];
					      }
					    } // if (iscale_dbols == 1)
					//
					//  THIS CHOICE OF SCALING MAKES EACH NONZERO COLUMN
					//  HAVE EUCLIDEAN LENGTH EQUAL TO ONE.
					//
					    else if ( iscale_dbols == 2 ) {
                          arr = new double[mrows+1];
                          for (k = 1; k <= mrows; k++) {
                        	  arr[k] = w[k][j];
                          }
					      rw[j] = dnrm2(mrows,arr,1);
					      if ( rw[j] == 0.0 ) {
					          rw[j] = 1.0;
					      }
					      else {
					          rw[j] = 1.0/rw[j];
					      }
					    } // else if (iscale_dbols == 2)
					//
					//  THIS CASE EFFECTIVELY SUPPRESSES SCALING BY SETTING
					//  THE SCALING MATRIX TO THE IDENTITY MATRIX.
					//
					    else if ( iscale_dbols == 3 ) {
                          for (k = 1; k <= ncols; k++) {
					          rw[k] = 1.0;
                          }
					      break;
					    } // else if (iscale_dbols == 3)

					    else if ( iscale_dbols == 4 ) {

					      break;
					 
					    } // else if (iscale_dbols == 4)

				  } // for (j = 1; j <= ncols; j++)
					//
					//  SOLVE BOUNDED LEAST SQUARES PROBLEM
					//
					//  INITIALIZE IBASIS(*), J=1,NCOLS, AND IBB(*), J=1,NCOLS,
					//  TO =J,AND =1, FOR USE IN DBOLSM( ).
					//
					  for (j = 1; j <= ncols; j++) {
					      iw[j] = j;
					      iw[j+ncols] = 1;
					      rw[3*ncols+j] = bl[j];
					      rw[4*ncols+j] = bu[j];
					  } // for (j = 1; j <= ncols; j++)
                      blArr = new double[ncols+1];
                      buArr = new double[ncols+1];
                      ioptArr = new int[iopt.length - lopt_dbols + 1];
                      rwArr = new double[ncols+1];
                      wwArr = new double[ncols+1];
                      ibbArr = new int[ncols+1];
                      for (k = 1; k <= ncols; k++) {
                    	  blArr[k] = rw[3*ncols+k];
                    	  buArr[k] = rw[4*ncols+k];
                    	  rwArr[k] = rw[ncols+k];
                    	  wwArr[k] = rw[2*ncols+k];
                    	  ibbArr[k] = iw[ncols+k];
                      }
                      for (k = 1; k <= iopt.length - lopt_dbols; k++) {
                    	  ioptArr[k] = iopt[lopt_dbols+k-1];
                      }
					  dbolsm(w,mdw,mrows,ncols,blArr,buArr,ind,
					    ioptArr,x,rnorm,mode,rwArr,wwArr,rw,iw,ibbArr);
					  for (k = 1; k <= ncols; k++) {
						  rw[ncols+k] = rwArr[k];
						  rw[2*ncols+k] = wwArr[k];
					  }

					  igo_dbols = 0;
					  return;

			  } // else if (igo_dbols == 2)
			  else {
			    return;
			  }
			
    } // dbols
	
	private void dbolsm ( double w[][], int mdw, int minput, int ncols, double bl[], double bu[],
			              int ind[], int iopt[], double x[], double rnorm[], int mode[], double rw[],
			              double ww[], double scl[], int ibasis[], int ibb[] ) {

			/*****************************************************************************80
			!
			!! DBOLSM solves E*X = F in the least squares sense with bounds on some X values.
			!
			!***BEGIN PROLOGUE  DBOLSM
			!***DESCRIPTION
			!
			!          Solve E*X = F (least squares sense) with bounds on
			!            selected X values.
			!     The user must have dimension statements of the form:
			!
			!       DIMENSION W(MDW,NCOLS+1), BL(NCOLS), BU(NCOLS),
			!      * X(NCOLS+NX), RW(NCOLS), WW(NCOLS), SCL(NCOLS)
			!       integer IND(NCOLS), IOPT(1+NI), IBASIS(NCOLS), IBB(NCOLS)
			!
			!     (here NX=number of extra locations required for options 1,...,7;
			!     NX=0 for no options; here NI=number of extra locations possibly
			!     required for options 1-7; NI=0 for no options; NI=14 if all the
			!     options are simultaneously in use.)
			!
			!    INPUT
			!    -----
			!
			!    --------------------
			!    W(MDW,*),MROWS,NCOLS
			!    --------------------
			!     The array w(*,*) contains the matrix [E:F] on entry. The matrix
			!     [E:F] has MROWS rows and NCOLS+1 columns. This data is placed in
			!     the array W(*,*) with E occupying the first NCOLS columns and the
			!     right side vector F in column NCOLS+1. The row dimension, MDW, of
			!     the array W(*,*) must satisfy the inequality MDW >= MROWS.
			!     Other values of MDW are errors. The values of MROWS and NCOLS
			!     must be positive. Other values are errors.
			!
			!    ------------------
			!    BL(*),BU(*),IND(*)
			!    ------------------
			!     These arrays contain the information about the bounds that the
			!     solution values are to satisfy. The value of IND(J) tells the
			!     type of bound and BL(J) and BU(J) give the explicit values for
			!     the respective upper and lower bounds.
			!
			!    1.    For IND(J)=1, require X(J) >= BL(J).
			!    2.    For IND(J)=2, require X(J) <= BU(J).
			!    3.    For IND(J)=3, require X(J) >= BL(J) and
			!                                X(J) <= BU(J).
			!    4.    For IND(J)=4, no bounds on X(J) are required.
			!     The values of BL(*),BL(*) are modified by the subprogram. Values
			!     other than 1,2,3 or 4 for IND(J) are errors. In the case IND(J)=3
			!     (upper and lower bounds) the condition BL(J)  >  BU(J) is an
			!     error.
			!
			!    -------
			!    IOPT(*)
			!    -------
			!     This is the array where the user can specify nonstandard options
			!     for DBOLSM( ). Most of the time this feature can be ignored by
			!     setting the input value IOPT(1)=99. Occasionally users may have
			!     needs that require use of the following subprogram options. For
			!     details about how to use the options see below: IOPT(*) CONTENTS.
			!
			!     Option Number   Brief Statement of Purpose
			!     ----- ------   ----- --------- -- -------
			!           1         Move the IOPT(*) processing pointer.
			!           2         Change rank determination tolerance.
			!           3         Change blow-up factor that determines the
			!                     size of variables being dropped from active
			!                     status.
			!           4         Reset the maximum number of iterations to use
			!                     in solving the problem.
			!           5         The data matrix is triangularized before the
			!                     problem is solved whenever (NCOLS/MROWS)  <
			!                     FAC. Change the value of FAC.
			!           6         Redefine the weighting matrix used for
			!                     linear independence checking.
			!           7         Debug output is desired.
			!          99         No more options to change.
			!
			!    ----
			!    X(*)
			!    ----
			!     This array is used to pass data associated with options 1,2,3 and
			!     5. Ignore this input parameter if none of these options are used.
			!     Otherwise see below: IOPT(*) CONTENTS.
			!
			!    ----------------
			!    IBASIS(*),IBB(*)
			!    ----------------
			!     These arrays must be initialized by the user. The values
			!         IBASIS(J)=J, J=1,...,NCOLS
			!         IBB(J)   =1, J=1,...,NCOLS
			!     are appropriate except when using nonstandard features.
			!
			!    ------
			!    SCL(*)
			!    ------
			!     This is the array of scaling factors to use on the columns of the
			!     matrix E. These values must be defined by the user. To suppress
			!     any column scaling set SCL(J)=1.0, J=1,...,NCOLS.
			!
			!    OUTPUT
			!    ------
			!
			!    ----------
			!    X(*),RNORM
			!    ----------
			!     The array X(*) contains a solution (if MODE >=0 or  == -22) for
			!     the constrained least squares problem. The value RNORM is the
			!     minimum residual vector length.
			!
			!    ----
			!    MODE
			!    ----
			!     The sign of mode determines whether the subprogram has completed
			!     normally, or encountered an error condition or abnormal status.
			!     A value of MODE >= 0 signifies that the subprogram has completed
			!     normally. The value of MODE (>= 0) is the number of variables
			!     in an active status: not at a bound nor at the value ZERO, for
			!     the case of free variables. A negative value of MODE will be one
			!     of the 20 cases -40,-39,...,-22, or -1. Values  <  -1 correspond
			!     to an abnormal completion of the subprogram. To understand the
			!     abnormal completion codes see below: ERROR MESSAGES for DBOLSM( )
			!     An approximate solution will be returned to the user only when
			!     max. iterations is reached, MODE=-22.
			!
			!    -----------
			!    RW(*),WW(*)
			!    -----------
			!     These are working arrays each with NCOLS entries. The array RW(*)
			!     contains the working (scaled, nonactive) solution values. The
			!     array WW(*) contains the working (scaled, active) gradient vector
			!     values.
			!
			!    ----------------
			!    IBASIS(*),IBB(*)
			!    ----------------
			!     These arrays contain information about the status of the solution
			!     when MODE >= 0. The indices IBASIS(K), K=1,...,MODE, show the
			!     nonactive variables; indices IBASIS(K), K=MODE+1,..., NCOLS are
			!     the active variables. The value (IBB(J)-1) is the number of times
			!     variable J was reflected from its upper bound. (normally the user
			!     can ignore these parameters.)
			!
			!    IOPT(*) CONTENTS
			!    ------- --------
			!     The option array allows a user to modify internal variables in
			!     the subprogram without recompiling the source code. A central
			!     goal of the initial software design was to do a good job for most
			!     people. Thus the use of options will be restricted to a select
			!     group of users. The processing of the option array proceeds as
			!     follows: a pointer, here called LP, is initially set to the value
			!     1. The value is updated as the options are processed.  At the
			!     pointer position the option number is extracted and used for
			!     locating other information that allows for options to be changed.
			!     The portion of the array IOPT(*) that is used for each option is
			!     fixed; the user and the subprogram both know how many locations
			!     are needed for each option. A great deal of error checking is
			!     done by the subprogram on the contents of the option array.
			!     Nevertheless it is still possible to give the subprogram optional
			!     input that is meaningless. For example some of the options use
			!     the location X(NCOLS+IOFF) for passing data. The user must manage
			!     the allocation of these locations when more than one piece of
			!     option data is being passed to the subprogram.
			!
			!   1
			!   -
			!     Move the processing pointer (either forward or backward) to the
			!     location IOPT(LP+1). The processing pointer is moved to location
			!     LP+2 of IOPT(*) in case IOPT(LP)=-1.  For example to skip over
			!     locations 3,...,NCOLS+2 of IOPT(*),
			!
			!       IOPT(1)=1
			!       IOPT(2)=NCOLS+3
			!       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
			!       IOPT(NCOLS+3)=99
			!       CALL DBOLSM( )
			!
			!     CAUTION: Misuse of this option can yield some very hard
			!     -to-find bugs.  Use it with care.
			!
			!   2
			!   -
			!     The algorithm that solves the bounded least squares problem
			!     iteratively drops columns from the active set. This has the
			!     effect of joining a new column vector to the QR factorization of
			!     the rectangular matrix consisting of the partially triangularized
			!     nonactive columns. After triangularizing this matrix a test is
			!     made on the size of the pivot element. The column vector is
			!     rejected as dependent if the magnitude of the pivot element is
			!     <= TOL* magnitude of the column in components strictly above
			!     the pivot element. Nominally the value of this (rank) tolerance
			!     is TOL = DRELPR, where DRELPR is relative machine
			!     precision. To change only the value of TOL, for example,
			!
			!       X(NCOLS+1)=TOL
			!       IOPT(1)=2
			!       IOPT(2)=1
			!       IOPT(3)=99
			!       CALL DBOLSM()
			!
			!     Generally, if LP is the processing pointer for IOPT(*),
			!
			!       X(NCOLS+IOFF)=TOL
			!       IOPT(LP)=2
			!       IOPT(LP+1)=IOFF
			!        .
			!       CALL DBOLSM()
			!
			!     The required length of IOPT(*) is increased by 2 if option 2 is
			!     used; The required length of X(*) is increased by 1. A value of
			!     IOFF <= 0 is an error. A value of TOL .le. DRELPR gives a
			!     warning message; it is not considered an error.
			!     Here DRELPR is the relative machine precision.
			!
			!   3
			!   -
			!     A solution component is left active (not used) if, roughly
			!     speaking, it seems too large. Mathematically the new component is
			!     left active if the magnitude is >=((vector norm of F)/(matrix
			!     norm of E))/BLOWUP. Nominally the factor BLOWUP = SQRT(DRELPR)
			!     where DRELPR is the relative machine precision. To change only
			!     the value of BLOWUP, for example,
			!
			!       X(NCOLS+2)=BLOWUP
			!       IOPT(1)=3
			!       IOPT(2)=2
			!       IOPT(3)=99
			!       CALL DBOLSM()
			!
			!     Generally, if LP is the processing pointer for IOPT(*),
			!
			!       X(NCOLS+IOFF)=BLOWUP
			!       IOPT(LP)=3
			!       IOPT(LP+1)=IOFF
			!        .
			!       CALL DBOLSM()
			!
			!     The required length of IOPT(*) is increased by 2 if option 3 is
			!     used; the required length of X(*) is increased by 1. A value of
			!     IOFF <= 0 is an error. A value of BLOWUP .le. 0.0 is an error.
			!
			!   4
			!   -
			!     Normally the algorithm for solving the bounded least squares
			!     problem requires between NCOLS/3 and NCOLS drop-add steps to
			!     converge. (this remark is based on examining a small number of
			!     test cases.) The amount of arithmetic for such problems is
			!     typically about twice that required for linear least squares if
			!     there are no bounds and if plane rotations are used in the
			!     solution method. Convergence of the algorithm, while
			!     mathematically certain, can be much slower than indicated. To
			!     avoid this potential but unlikely event ITMAX drop-add steps are
			!     permitted. Nominally ITMAX=5*(MAX(MROWS,NCOLS)). To change the
			!     value of ITMAX, for example,
			!
			!       IOPT(1)=4
			!       IOPT(2)=ITMAX
			!       IOPT(3)=99
			!       CALL DBOLSM()
			!
			!     Generally, if LP is the processing pointer for IOPT(*),
			!
			!       IOPT(LP)=4
			!       IOPT(LP+1)=ITMAX
			!        .
			!       CALL DBOLSM()
			!
			!     The value of ITMAX must be  >  0. Other values are errors. Use
			!     of this option increases the required length of IOPT(*) by 2.
			!
			!   5
			!   -
			!     For purposes of increased efficiency the MROWS by NCOLS+1 data
			!     matrix [E:F] is triangularized as a first step whenever MROWS
			!     satisfies FAC*MROWS  >  NCOLS. Nominally FAC=0.  To change the
			!     value of FAC,
			!
			!       X(NCOLS+3)=FAC
			!       IOPT(1)=5
			!       IOPT(2)=3
			!       IOPT(3)=99
			!       CALL DBOLSM()
			!
			!     Generally, if LP is the processing pointer for IOPT(*),
			!
			!       X(NCOLS+IOFF)=FAC
			!       IOPT(LP)=5
			!       IOPT(LP+1)=IOFF
			!        .
			!       CALL DBOLSM()
			!
			!     The value of FAC must be nonnegative. Other values are errors.
			!     Using the value FAC=0.0 suppresses the initial triangularization step.
			!     Use of this option increases the required length of IOPT(*) by 2;
			!     The required length of of X(*) is increased by 1.
			!
			!   6
			!   -
			!     The norm used in testing the magnitudes of the pivot element
			!     compared to the mass of the column above the pivot line can be
			!     changed. The type of change that this option allows is to weight
			!     the components with an index larger than MVAL by the parameter
			!     WT. Normally MVAL=0 and WT=1. To change both the values MVAL and
			!     WT, where LP is the processing pointer for IOPT(*),
			!
			!       X(NCOLS+IOFF)=WT
			!       IOPT(LP)=6
			!       IOPT(LP+1)=IOFF
			!       IOPT(LP+2)=MVAL
			!
			!     Use of this option increases the required length of IOPT(*) by 3.
			!     The length of X(*) is increased by 1. Values of MVAL must be
			!     nonnegative and not greater than MROWS. Other values are errors.
			!     The value of WT must be positive. Any other value is an error. If
			!     either error condition is present a message will be printed.
			!
			!   7
			!   -
			!     Debug output, showing the detailed add-drop steps for the
			!     constrained least squares problem, is desired. This option is
			!     intended to be used to locate suspected bugs.  To print,
			!
			!       IOPT(LP)=7
			!
			!   99
			!   --
			!     There are no more options to change.
			!
			!     The values for options are 1,...,7,99, and are the only ones
			!     permitted. Other values are errors. Options -99,-1,...,-7 mean
			!     that the repective options 99,1,...,7 are left at their default
			!     values. An example is the option to modify the (rank) tolerance:
			!
			!       X(NCOLS+1)=TOL
			!       IOPT(1)=-2
			!       IOPT(2)=1
			!       IOPT(3)=99
			!
			!***END PROLOGUE  DBOLSM
			!
			!     PURPOSE
			!     -------
			!     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE BOUNDED
			!     LEAST SQUARES PROBLEM.  THE PROBLEM SOLVED HERE IS:
			!
			!     SOLVE E*X =  F  (LEAST SQUARES SENSE)
			!     WITH BOUNDS ON SELECTED X VALUES.
			*/

			  /*double alpha;
			  double beta;
			  double big;
			  //double bl(ncols)
			  double bou;
			  //double bu(ncols)
			  double cl1;
			  double cl2;
			  double cl3;
			  boolean cnz;
			  double colabv;
			  double colblo;
			  boolean constr;
			  double fac;
			  boolean found;
			  int i;
			  int icase;
			  int idum;
			  int igopr;
			  //int ind(ncols)
			  int ioff;
			  int ip;
			  int iprint;
			  int itemp;
			  int iter;
			  int itmax;
			  int j;
			  int jbig;
			  int jcol;
			  int jdrop;
			  int jdrop1;
			  int jdrop2;
			  int jp;
			  int lds;
			  int level;
			  int lgopr;
			  int lp;
			  int mrows;
			  int mval;
			  int nerr;
			  int nlevel;
			  int nsetb;
			  double rdum;
			  double sc;
			  double ss;
			  double t;
			  double t1;
			  double t2;
			  double tolind;
			  double tolsze;
			  //double w(mdw,*)
			  double wlarge;
			  double wla;
			  double wlb;
			  double wt;
			  double xnew;
			  boolean do50 = false;
			  boolean do460 = false;
			  boolean do570 = false;
			//
			//  Statement function:
			//
			  //inext[idum] = Math.min(idum+1,mrows);

			  level = 1;
			//
			//  VERIFY THAT THE PROBLEM DIMENSIONS ARE DEFINED PROPERLY.
			//
			  if ( minput<=0) {
			    nerr = 31;
			    xerrwv("dbolsm(). the number of rows=(i1) must be positive.",
			      nerr,level,1,minput,idum,0,rdum,rdum);
			    mode[0] = -nerr;
				return;
			  } // if (minput <= 0)

			  if ( ncols<=0) {
			      nerr = 32;
			      xerrwv("dbolsm(). the number of cols.=(i1) must be positive.",
			             nerr,level,1,ncols,idum,0,rdum,rdum);
			      mode[0] = -nerr;
			      return;
			  } // if (ncols <= 0)

			  if ( mdw < minput) {
			      nerr = 33;
			      xerrwv("dbolsm(). the row dimension of w(,)=(i1) must be >= the number of rows=(i2).",
			    		  nerr,level,2,mdw,mrows,0,rdum,rdum);
			      
			      mode[0] = -nerr;
				  return;
	          } // if (mdw < minput)
			//
			//  VERIFY THAT BOUND INFORMATION IS CORRECT.
			//
			  for (j = 1; j <=ncols; j++) {
			     if ( ind[j] < 1 || ind[j] > 4) {
			         nerr = 34;
			         xerrwv("dbolsm(). for j=(i1) the constraint indicator must be1-4.",
			                 nerr,level,2,j,ind[j],0,rdum,rdum);
			         mode[0] = -nerr;
			         return;
			     } // if (ind[j] < 1 || ind[j] > 4)
			  } // for (j = 1; j <= ncols; j++)

			  for (j = 1; j <= ncols; j++) {
			     if ( ind[j] == 3) {
			         if ( bu[j] < bl[j]) {
			             nerr = 35;
			             xerrwv("dbolsm(). for j=(i1) the lower bound=(r1) is >  the upper bound=(r2).",
			            		 nerr,level,1,j,idum,2,bl[j],bu[j]);
			             mode[0] = -nerr;
			             return;
			         } // if (bu[j] < bl[j])
			     } // if (ind[j] == 3)
			  } // for (j = 1; j <= ncols; j++)
			//
			//  CHECK THAT PERMUTATION AND POLARITY ARRAYS HAVE BEEN SET.
			//
			  for (j = 1; j <= ncols; j++) {
			     if ( ibasis[j] < 1 || ibasis[j] > ncols) {
			         nerr = 36;
			         xerrwv("dbolsm(). the input order of columns=(i1) is not between 1 and ncols=(i2).",
			        		 nerr,level,2,ibasis[j],ncols,0,rdum,rdum);
			         mode[0] = -nerr;
			         return;
			     } // if ( ibasis[j] < 1 || ibasis[j] > ncols)

			     if ( ibb[j]<=0) {
			         nerr = 37;
			         xerrwv("dbolsm(). the bound polarity flag in component j=(i1) must be positive. now=(i2).",
			        		nerr,level,2,j,ibb[j], 0,rdum,rdum);
			         mode[0] = -nerr;
				     return;

			     } // if (ibb[j] <= 0)
			  } // for (j = 1; j <= ncols; j++)
			//
			//  PROCESS OPTION ARRAY
			//
			  do570 = true;
	  loop: while (true) {
			//
			//  INITIALIZE VARIABLES AND DATA VALUES
			//

			  if (do50) {
				  if ( iprint > 0) {
				      call dmout(mrows,ncols+1,mdw,w,'('' pretri. input matrix'')',-4)
				      call dvout(ncols,bl,'('' lower bounds'')',-4)
				      call dvout(ncols,bu,'('' upper bounds'')',-4)
				  }
			  } // if (do50)

			   60 continue
			  iter = iter + 1
			  if ( iter<=itmax) go to 80
			  nerr = 22
			  call xerrwv('dbolsm(). more than (i1)=itmax iterations solving bounded ' // &
			    'least squares problem.', &
			    nerr,level,1,itmax,idum,0,rdum,rdum)
			!
			!  RESCALE AND TRANSLATE VARIABLES
			!
			  igopr = 1
			  go to 130

			   70 continue

			   mode[0] = -nerr;
			   return;

			   80 continue
			!
			!  FIND A VARIABLE TO BECOME NON-ACTIVE
			!
			  go to 180

			   90 continue

			  if ( found ) go to 110
			!
			!  RESCALE AND TRANSLATE VARIABLES
			!
			  igopr = 2
			  go to 130

			  100 continue
			  mode = nsetb
			  return

			  110 continue
			!
			!  MAKE MOVE AND UPDATE FACTORIZATION
			!
			  go to 280

			  120 continue
			  go to 60
			!
			!  RESCALE AND TRANSLATE VARIABLES
			!
			  130 continue

			  rw(1:nsetb) = x(1:nsetb)

			  x(1:ncols) = 0.0D+00

			  do j = 1,nsetb
			     jcol = abs(ibasis(j))
			     x(jcol) = rw(j)*abs(scl(jcol))
			  end do

			  do j = 1,ncols
			     if ( mod(ibb(j),2) == 0) x(j) = bu(j) - x(j)
			  end do

			  do j = 1,ncols
			     jcol = ibasis(j)
			     if ( jcol < 0) x(-jcol) = bl(-jcol) + x(-jcol)
			  end do

			  do j = 1,ncols
			     if ( scl(j) < 0.0D+00 ) x(j) = -x(j)
			  end do

			  call dscal ( mrows-mval, wt, w(Math.min(mval+1,mrows),ncols+1), 1 )

			  rnorm = dnrm2(mrows-max(nsetb,mval),w(Math.min((max(nsetb,mval) + 1),mrows),ncols+1),1)

			  go to (70,100),igopr
			!
			!  FIND A VARIABLE TO BECOME NON-ACTIVE
			!
			  180 continue
			!
			!  COMPUTE (NEGATIVE) OF GRADIENT VECTOR, W=(TRANSPOSE OF E)*(F-E*X).
			!
			  ww(1:ncols) = 0.0D+00

			  do j = nsetb + 1, ncols
			     jcol = abs ( ibasis(j) )
			     ww(j) = ddot(mrows-nsetb,w(Math.min(nsetb+1,mrows),j),1, &
			             w(Math.min(nsetb+1,mrows),ncols+1),1)*abs(scl(jcol))
			  end do

			  if ( iprint > 0) then
			      call dvout(ncols,ww,'('' Gradient values'')',-4)
			      call ivout(ncols,ibasis,'('' internal variable order'')',-4)
			      call ivout(ncols,ibb,'('' bound polarity'')',-4)
			  end if

			  200 continue
			!
			!  IF ACTIVE SET = NUMBER OF TOTAL ROWS, QUIT.
			!
			  if ( nsetb == mrows) then
			      found = .false.
			      go to 90
			  end if
			!
			!  CHOOSE AN EXTREMAL COMPONENT OF GRADIENT VECTOR
			!  FOR A CANDIDATE TO BECOME NON-ACTIVE.
			!
			  wlarge = -big
			  jbig = 0
			  cnz = .false.

			  do j = nsetb + 1,ncols

			    t = ww(j)
			!
			!  SKIP LOOKING AT COMPONENTS FLAGGED AS NON-CANDIDATES.
			!
			    if ( t == big) then
			      cycle
			    end if

			    itemp = ibasis(j)
			    jcol = abs(itemp)
			    if ( nsetb < mval) then
			         cl1 = dnrm2(nsetb,w(1,j),1)
			         cl2 = dnrm2(mval-nsetb,w(Math.min(nsetb+1,mrows),j),1)
			         colabv = cl1
			         colblo = cl2
			    else
			         cl1 = dnrm2(mval,w(1,j),1)
			         cl2 = abs(wt)*dnrm2(nsetb-mval,w(Math.min(mval+1,mrows),j),1)
			         cl3 = abs(wt)*dnrm2(mrows-nsetb,w(Math.min(nsetb+1,mrows),j),1)
			         call drotg(cl1,cl2,sc,ss)
			         colabv = abs(cl1)
			         colblo = cl3
			    end if

			    if ( itemp < 0) then
			      if ( mod(ibb(jcol),2) == 0) t = -t
			!
			!  SKIP LOOKING AT COMPONENTS THAT WOULD NOT DECREASE OBJECTIVE.
			!
			      if ( t < 0.0D+00 ) then
			        cycle
			      end if

			    end if
			!
			!  THIS IS A COLUMN PIVOTING STEP THAT MAXIMIZES THE RATIO OF
			!  COLUMN MASS ON AND BELOW THE PIVOT LINE RELATIVE TO THAT
			!  STRICTLY ABOVE THE PIVOT LINE.
			!
			     if ( colabv == 0.0D+00 .and. .not. cnz) then
			         t = colblo*abs(scl(jcol))
			         if ( wlarge < t) then
			             wlarge = t
			             jbig = j
			         end if
			     else
			         if ( .not. cnz) then
			             wla = 0.0D+00
			             wlb = 0.0D+00
			             cnz = .true.
			         end if

			       if ( sqrt(colblo)*sqrt(wla) >= sqrt(colabv)*sqrt(wlb)) then
			            wlb=colblo
			            wla=colabv
			            jbig=j
			       end if

			     end if

			  end do

			  if ( jbig == 0) then
			      found = .false.
			      if ( iprint > 0) then
			        write ( *, '(a)' ) '  Found no variable to enter.'
			      end if

			      go to 90

			  end if
			!
			!  SEE IF THE INCOMING COL. IS SUFFICIENTLY INDEPENDENT.
			!  THIS TEST IS MADE BEFORE AN ELIMINATION IS PERFORMED.
			!
			  if ( iprint > 0) then
			    write ( *, '(a,i6)' ) '  Try to bring in column ', jbig
			  end if

			  if ( cnz) then

			    if ( wlb<=wla*tolind) then
			      found = .false.
			      if ( iprint > 0) then
			        write ( *, '(a)' ) '  Variable is dependent, not used.'
			      end if

			      ww(jbig) = big
			      go to 200

			    end if

			  end if
			!
			!  SWAP MATRIX COLS. NSETB+1 AND JBIG, PLUS POINTER INFO., AND
			!  GRADIENT VALUES.
			!
			  nsetb = nsetb + 1
			  if ( nsetb/=jbig) then
			      call dswap(mrows,w(1,nsetb),1,w(1,jbig),1)
			      call dswap(1,ww(nsetb),1,ww(jbig),1)
			      itemp = ibasis(nsetb)
			      ibasis(nsetb) = ibasis(jbig)
			      ibasis(jbig) = itemp
			  end if
			!
			!  ELIMINATE ENTRIES BELOW THE PIVOT LINE IN COL. NSETB.
			!
			  if ( mrows > nsetb) then

			      do i = mrows,nsetb + 1,-1
			         if ( i /= mval+1 ) then
			           call drotg(w(i-1,nsetb),w(i,nsetb),sc,ss)
			           w(i,nsetb) = 0.0D+00
			           call drot(ncols-nsetb+1,w(i-1,nsetb+1),mdw,w(i,nsetb+1),mdw,sc,ss)
			         end if
			      end do

			      if ( (mval>=nsetb) .and. (mval < mrows)) then
			          t = w(nsetb,nsetb)
			          if ( t/= 0.0D+00 ) then
			              t = wt*w(mval+1,nsetb)/t
			          else
			              t = big
			          end if

			          if ( tolind*abs(t) > one) then
			              call dswap(ncols-nsetb+2,w(nsetb,nsetb),mdw,w(mval+1,nsetb),mdw)
			              call dscal(ncols-nsetb+2,wt,w(nsetb,nsetb),mdw)
			              call dscal(ncols-nsetb+2,one/wt,w(mval+1,nsetb),mdw)
			          end if

			          call drotg(w(nsetb,nsetb),w(mval+1,nsetb),sc,ss)
			          w(mval+1,nsetb) = 0.0D+00
			          call drot(ncols-nsetb+1,w(nsetb,nsetb+1),mdw, &
			                   w(mval+1,nsetb+1),mdw,sc,ss)
			      end if

			  end if

			  if ( w(nsetb,nsetb) == 0.0D+00 ) then
			      ww(nsetb) = big
			      nsetb = nsetb - 1
			      if ( iprint > 0) then
			        write ( *, '(a)' ) '  Pivot is zero, not used.'
			      end if

			      go to 200
			  end if
			!
			!  CHECK THAT NEW VARIABLE IS MOVING IN THE RIGHT DIRECTION.
			!
			  itemp = ibasis(nsetb)
			  jcol = abs(itemp)
			  xnew = (w(nsetb,ncols+1)/w(nsetb,nsetb))/abs(scl(jcol))

			  if ( itemp < 0) then
			      if ( ww(nsetb)>= 0.0D+00 .and. xnew<= 0.0D+00 ) go to 230
			      if ( ww(nsetb)<= 0.0D+00 .and. xnew>= 0.0D+00 ) go to 230
			  end if

			  go to 240

			  230 continue
			  ww(nsetb) = big
			  nsetb = nsetb - 1
			  if ( iprint > 0) then
			    write ( *, '(a)' ) '  Variable has bad direction, not used.'
			  end if

			  go to 200

			  240 continue
			  found = .true.
			  go to 250

			  250 continue

			  go to 90
			!
			!  SOLVE THE TRIANGULAR SYSTEM
			!
			  260 continue
			  call dcopy(nsetb,w(1,ncols+1),1,rw,1)

			  do j = nsetb,1,-1
			     rw(j) = rw(j)/w(j,j)
			     jcol = abs(ibasis(j))
			     t = rw(j)
			     if ( mod(ibb(jcol),2) == 0) rw(j) = -rw(j)
			     call daxpy(j-1,-t,w(1,j),1,rw,1)
			     rw(j) = rw(j)/abs(scl(jcol))
			  end do

			  if ( iprint > 0) then
			      call dvout(nsetb,rw,'('' soln. values'')',-4)
			      call ivout(nsetb,ibasis,'('' cols. used'')',-4)
			  end if

			  go to (290,430),lgopr
			!
			!  MAKE MOVE AND UPDATE FACTORIZATION
			!
			  280 continue
			!
			!  SOLVE THE TRIANGULAR SYSTEM
			!
			  lgopr = 1
			  go to 260

			  290 continue
			!
			!  SEE IF THE UNCONSTRAINED SOL. (OBTAINED BY SOLVING THE
			!  TRIANGULAR SYSTEM) SATISFIES THE PROBLEM BOUNDS.
			!
			  alpha = 2.0D+00
			  beta = 2.0D+00
			  x(nsetb) = 0.0D+00

			  do j = 1,nsetb

			     itemp = ibasis(j)
			     jcol = abs(itemp)
			     t1 = 2.0D+00
			     t2 = 2.0D+00

			     if ( itemp < 0) then
			         bou = 0.0D+00
			     else
			         bou = bl(jcol)
			     end if

			     if ( (-bou)/=big) bou = bou/abs(scl(jcol))
			     if ( rw(j)<=bou) t1 = (x(j)-bou)/ (x(j)-rw(j))
			     bou = bu(jcol)
			     if ( bou/=big) bou = bou/abs(scl(jcol))
			     if ( rw(j)>=bou) t2 = (bou-x(j))/ (rw(j)-x(j))
			!
			!  IF NOT, THEN COMPUTE A STEP LENGTH SO THAT THE
			!  VARIABLES REMAIN FEASIBLE.
			!
			     if ( t1 < alpha) then
			         alpha = t1
			         jdrop1 = j
			     end if

			     if ( t2 < beta) then
			         beta = t2
			         jdrop2 = j
			     end if

			  end do

			  constr = alpha < 2.0D+00 .or. beta < 2.0D+00
			  if ( constr) go to 310
			!
			!  ACCEPT THE CANDIDATE BECAUSE IT SATISFIES THE STATED BOUNDS
			!  ON THE VARIABLES.
			!
			  x(1:nsetb) = rw(1:nsetb)

			  go to 120

			  310 continue
			!
			!  TAKE A STEP THAT IS AS LARGE AS POSSIBLE WITH ALL
			!  VARIABLES REMAINING FEASIBLE.
			!
			  do j = 1,nsetb
			     x(j) = x(j) + min(alpha,beta)* (rw(j)-x(j))
			  end do

			  if ( alpha<=beta) then
			      jdrop2 = 0

			  else
			      jdrop1 = 0
			  end if

			  330 if ( jdrop1+jdrop2 > 0 .and. nsetb > 0) go to 340
			  go to 450

			  340 continue

			  jdrop = jdrop1 + jdrop2
			  itemp = ibasis(jdrop)
			  jcol = abs(itemp)
			  if ( jdrop2 > 0) then
			!
			!  VARIABLE IS AT AN UPPER BOUND.  SUBTRACT MULTIPLE OF THIS COL.
			!  FROM RIGHT HAND SIDE.
			!
			      t = bu(jcol)
			      if ( itemp > 0) then

			          bu(jcol) = t - bl(jcol)
			          bl(jcol) = -t
			          itemp = -itemp
			          scl(jcol) = -scl(jcol)
			          w(1:jdrop,jdrop) = -w(1:jdrop,jdrop)

			      else
			          ibb(jcol) = ibb(jcol) + 1
			          if ( mod(ibb(jcol),2) == 0) t = -t
			      end if
			!
			!  VARIABLE IS AT A LOWER BOUND.
			!
			  else
			      if ( itemp < 0.0D+00 ) then
			          t = 0.0D+00
			      else
			          t = -bl(jcol)
			          bu(jcol) = bu(jcol) + t
			          itemp = -itemp
			      end if

			  end if

			  call daxpy(jdrop,t,w(1,jdrop),1,w(1,ncols+1),1)
			!
			!  MOVE CERTAIN COLS. LEFT TO ACHIEVE UPPER HESSENBERG FORM.
			!
			  call dcopy(jdrop,w(1,jdrop),1,rw,1)

			  do j = jdrop + 1,nsetb
			     ibasis(j-1) = ibasis(j)
			     x(j-1) = x(j)
			     call dcopy(j,w(1,j),1,w(1,j-1),1)
			  end do

			  ibasis(nsetb) = itemp
			  w(1,nsetb) = 0.0D+00
			  w(jdrop+1:mrows,nsetb) = 0.0D+00

			  call dcopy(jdrop,rw,1,w(1,nsetb),1)
			!
			!  TRANSFORM THE MATRIX FROM UPPER HESSENBERG FORM TO
			!  UPPER TRIANGULAR FORM.
			!
			  nsetb = nsetb - 1

			  do i = jdrop,nsetb
			!
			!  LOOK FOR SMALL PIVOTS AND AVOID MIXING WEIGHTED AND NONWEIGHTED ROWS.
			!
			     if ( i == mval) then
			         t = 0.0D+00
			         do j = i,nsetb
			            jcol = abs(ibasis(j))
			            t1 = abs(w(i,j)*scl(jcol))
			            if ( t1 > t) then
			                jbig = j
			                t = t1
			            end if
			         end do
			         go to 390
			     end if

			     call drotg(w(i,i),w(i+1,i),sc,ss)
			     w(i+1,i) = 0.0D+00
			     call drot(ncols-i+1,w(i,i+1),mdw,w(i+1,i+1),mdw,sc,ss)
			  end do

			  go to 420

			  390 continue
			!
			!  THE TRIANGULARIZATION IS COMPLETED BY GIVING UP
			!  THE HESSENBERG FORM AND TRIANGULARIZING A RECTANGULAR MATRIX.
			!
			  call dswap(mrows,w(1,i),1,w(1,jbig),1)
			  call dswap(1,ww(i),1,ww(jbig),1)
			  call dswap(1,x(i),1,x(jbig),1)
			  itemp = ibasis(i)
			  ibasis(i) = ibasis(jbig)
			  ibasis(jbig) = itemp
			  jbig = i
			  do j = jbig,nsetb
			     do i = j + 1,mrows
			        call drotg(w(j,j),w(i,j),sc,ss)
			        w(i,j) = 0.0D+00
			        call drot(ncols-j+1,w(j,j+1),mdw,w(i,j+1),mdw,sc,ss)
			     end do
			  end do

			  420 continue
			!
			!  SEE IF THE REMAINING COEFFICIENTS ARE FEASIBLE.  THEY SHOULD
			!  BE BECAUSE OF THE WAY MIN(ALPHA,BETA) WAS CHOSEN.  ANY THAT ARE
			!  NOT FEASIBLE WILL BE SET TO THEIR BOUNDS AND
			!  APPROPRIATELY TRANSLATED.
			!
			  jdrop1 = 0
			  jdrop2 = 0
			!
			!  SOLVE THE TRIANGULAR SYSTEM
			!
			  lgopr = 2
			  go to 260

			  430 continue
			  call dcopy(nsetb,rw,1,x,1)

			  do j = 1,nsetb

			     itemp = ibasis(j)
			     jcol = abs(itemp)

			     if ( itemp < 0) then
			         bou = 0.0D+00
			     else
			         bou = bl(jcol)
			     end if

			     if ( (-bou)/=big) bou = bou/abs(scl(jcol))

			     if ( x(j)<=bou) then
			         jdrop1 = j
			         go to 330
			     end if

			     bou = bu(jcol)

			     if ( bou/=big) bou = bou/abs(scl(jcol))

			     if ( x(j)>=bou) then
			         jdrop2 = j
			         go to 330
			     end if

			  end do

			  go to 330

			  450 continue

			  go to 120
			!
			!  INITIALIZE VARIABLES AND DATA VALUES
			!
			  460 continue
			!
			!  PRETRIANGULARIZE RECTANGULAR ARRAYS OF CERTAIN SIZES
			!  FOR INCREASED EFFICIENCY.
			!
			  if ( fac*minput > ncols) then
			      do j = 1,ncols + 1
			         do i = minput,j + mval + 1,-1
			            call drotg(w(i-1,j),w(i,j),sc,ss)
			            w(i,j) = 0.0D+00
			            call drot(ncols-j+1,w(i-1,j+1),mdw,w(i,j+1),mdw,sc,ss)
			         end do
			      end do
			      mrows = ncols + mval + 1
			  else
			      mrows = minput
			  end if
			!
			!  SET THE X(*) ARRAY TO ZERO SO ALL COMPONENTS ARE DEFINED.
			!
			  x(1:ncols) = 0.0D+00
			!
			!  THE ARRAYS IBASIS(*), IBB(*) ARE INITIALIZED BY THE CALLING
			!  PROGRAM UNIT.
			!  THE COL. SCALING IS DEFINED IN THE CALLING PROGRAM UNIT.
			!  'BIG' IS PLUS INFINITY ON THIS MACHINE.
			!
			  big = huge ( big )

			  do j = 1, ncols

			    if ( ind(j) == 1 ) then
			      bu(j) = big
			    else if ( ind(j) == 2 ) then
			      bl(j) = -big
			    else if ( ind(j) == 3 ) then

			    else if ( ind(j) == 4 ) then
			      bl(j) = -big
			      bu(j) = big
			    end if

			  end do

			  do j = 1,ncols

			     if ( (bl(j)<= 0.0D+00 .and. 0.0D+00 <= bu(j) .and. &
			        abs(bu(j)) < abs(bl(j))) .or. bu(j) < 0.0D+00 ) then
			         t = bu(j)
			         bu(j) = -bl(j)
			         bl(j) = -t
			         scl(j) = -scl(j)
			         w(1:mrows,j) = -w(1:mrows,j)
			     end if
			!
			!  INDICES IN SET T(=TIGHT) ARE DENOTED BY NEGATIVE VALUES OF IBASIS(*).
			!
			     if ( bl(j)>=0.0D+00 ) then
			         ibasis(j) = -ibasis(j)
			         t = -bl(j)
			         bu(j) = bu(j) + t
			         call daxpy(mrows,t,w(1,j),1,w(1,ncols+1),1)
			     end if

			  end do

			  nsetb = 0
			  iter = 0

			  go to 50
			!
			!  PROCESS OPTION ARRAY
			!
			  570 continue

			  if ( idope(5) == 1) then
			      fac = x(ncols+idope(1))
			      wt = x(ncols+idope(2))
			      mval = idope(3)
			  else
			      fac = 0.0D+00
			      wt = 1.0D+00
			      mval = 0
			  end if

			  tolind = sqrt( epsilon ( tolind ) )
			  tolsze = sqrt( epsilon ( tolsze ) )
			  itmax = 5 * max ( minput, ncols )
			  iprint = 0
			!
			!  CHANGES TO SOME PARAMETERS CAN OCCUR THROUGH THE OPTION
			!  ARRAY, IOPT(*).  PROCESS THIS ARRAY LOOKING CAREFULLY
			!  FOR INPUT DATA ERRORS.
			!
			  lp = 0
			  lds = 0

			  580 continue

			  lp = lp + lds
			!
			!  TEST FOR NO MORE OPTIONS.
			!
			  ip = iopt(lp+1)
			  jp = abs(ip)
			  if ( ip == 99) then
			      go to 590
			  else if ( jp == 99) then
			      lds = 1
			      go to 580
			  else if ( jp == 1) then
			!
			!  MOVE THE IOPT(*) PROCESSING POINTER.
			!
			      if ( ip > 0) then
			          lp = iopt(lp+2) - 1
			          lds = 0
			      else
			          lds = 2
			      end if

			      go to 580

			  else if ( jp == 2) then
			!
			!  CHANGE TOLERANCE FOR RANK DETERMINATION.
			!
			      if ( ip > 0) then
			          ioff = iopt(lp+2)
			          if ( ioff<=0) then
			              nerr = 24
			              call xerrwv('dbolsm(). the offset=(i1) beyond postion ' // &
			                'ncols=(i2) must be positive for option number 2.', &
			                nerr,level,2,ioff,ncols,0,rdum,rdum)
			              mode[0] = -nerr;
			              return;
			          end if

			          tolind = x(ncols+ioff)
			          if ( tolind < epsilon ( tolind ) ) then
			              nerr = 25
			              nlevel = 0
			              call xerrwv( 'dbolsm(). the tolerance for rank ' // &
			                'determination=(r1) is less than machine precision=(r2).', &
			                nerr,nlevel,0,idum,idum,2,tolind, epsilon ( tolind ) )
			          end if
			      end if

			      lds = 2
			      go to 580

			  else if ( jp == 3) then
			!
			!  CHANGE BLOWUP FACTOR FOR ALLOWING VARIABLES TO BECOME INACTIVE.
			!
			      if ( ip > 0) then
			          ioff = iopt(lp+2)
			          if ( ioff<=0) then
			              nerr = 26
			              call xerrwv( 'dbolsm(). the offset=(i1) beyond position ' // &
			                'ncols=(i2) must be postive for option number 3.', &
			                nerr,level,2,ioff,ncols,0,rdum,rdum)
			              mode[0] = -nerr;
					      return;
			          end if

			          tolsze = x(ncols+ioff)
			          if ( tolsze<= 0.0D+00 ) then
			              nerr = 27
			              call xerrwv('dbolsm(). the reciprocal of the blow-up factor ' // &
			                'for rejecting variables must be positive. now=(r1).', &
			                nerr,level,0,idum,idum,1,tolsze,rdum)
			              mode[0] = -nerr;
					      return;
			          end if
			      end if

			      lds = 2
			      go to 580

			  else if ( jp == 4) then
			!
			!  Change the maximum number of iterations allowed.
			!
			      if ( ip > 0) then
			          itmax = iopt(lp+2)
			          if ( itmax<=0) then
			              nerr = 28
			              call xerrwv('dbolsm(). the maximum number of iterations=(i1) ' // &
			                'must be positive.',nerr,level,1,itmax,idum,0,rdum,rdum)
			              mode[0] = -nerr;
					      return;
			          end if
			      end if

			      lds = 2
			      go to 580

			  else if ( jp == 5) then
			!
			!  CHANGE THE FACTOR FOR PRETRIANGULARIZING THE DATA MATRIX.
			!
			      if ( ip > 0) then
			          ioff = iopt(lp+2)
			          if ( ioff<=0) then
			              nerr = 29
			              call xerrwv('dbolsm(). the offset=(i1) beyond position ' // &
			                'ncols=(i2) must be postive for option number 5.', &
			                nerr,level,2,ioff,ncols,0,rdum,rdum)
			              mode[0] = -nerr;
					      return;
			          end if

			          fac = x(ncols+ioff)
			          if ( fac < 0.0D+00 ) then
			              nerr = 30
			              nlevel = 0
			              call xerrwv('dbolsm(). the factor (ncols/mrows) where ' // &
			                'pre-triangularizing is performed must be nonnegative. ' // &
			                'now=(r1).',nerr,nlevel,0,idum,idum,1,fac,rdum)
			              mode[0] = -nerr;
					      return;
			          end if
			      end if

			      lds = 2
			      go to 580
			  else if ( jp == 6) then
			!
			!  CHANGE THE WEIGHTING FACTOR (FROM ONE) TO APPLY TO COMPONENTS
			!  NUMBERED .GT. MVAL (INITIALLY SET TO 1.)  THIS TRICK IS NEEDED
			!  FOR APPLICATIONS OF THIS SUBPROGRAM TO THE HEAVILY WEIGHTED
			!  LEAST SQUARES PROBLEM THAT COME FROM EQUALITY CONSTRAINTS.
			!
			      if ( ip > 0) then
			        ioff = iopt(lp+2)
			        mval = iopt(lp+3)
			        wt = x(ncols+ioff)
			      end if

			      if ( mval < 0 .or. mval > minput .or. wt<= 0.0D+00 ) then
			          nerr = 38
			          nlevel = 0
			          call xerrwv('dbolsm(). the row separator to apply weighting (i1) ' // &
			            'must lie between 0 and mrows (i2). weight (r1) must be positive.', &
			            nerr,nlevel,2,mval,minput,1,wt,rdum)
			          mode[0] = -nerr;
					  return;
			      end if

			      lds = 3
			      go to 580
			!
			!  TURN ON DEBUG OUTPUT.
			!
			  else if ( jp == 7) then
			      if ( ip > 0) iprint = 1
			      lds = 1
			      go to 580
			  else
			      nerr = 23
			      call xerrwv('dbolsm. the option number=(i1) is not defined.', &
			           nerr,level,1,ip,idum,0,rdum,rdum)
			      mode[0] = -nerr;
				  return;
			  end if

			  590 continue

			  do460 = true;
	  } // loop while (true)

			  600 continue
			  mode[0] = -nerr;
			  return;*/
    } // dbolsm
	
	private void dcopy ( int n, double x[], int incx, double y[], int incy ) {

	/*****************************************************************************80
	!
	!! DCOPY copies one double precision vector into another.
	!
	!  Modified:
	!
	!    08 April 1999
	!
	!  Author:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
	!
	!  Reference:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input, integer N, the number of entries in the vector.
	!
	!    Input, double X(*), the vector to be copied into Y.
	!
	!    Input, integer INCX, the increment between successive entries of X.
	!
	!    Output, real double Y(*), the copy of X.
	!
	!    Input, integer INCY, the increment between successive elements of Y.
	*/

	  int i;
	  int ix;
	  int iy;

	  if ( n <= 0 ) {
		  
	  }

	  else if ( incx == 1 && incy == 1 ) {
        for (i = 1; i <= n; i++) {
	        y[i] = x[i];
        }
	  }
	    else {

	    if ( incx >= 0 ) {
	      ix = 1;
	    }
	    else {
	      ix = ( - n + 1 ) * incx + 1;
	    }

	    if ( incy >= 0 ) {
	      iy = 1;
	    }
	    else {
	      iy = ( - n + 1 ) * incy + 1;
	    }

	    for (i = 1; i <= n; i++) {
	      y[iy] = x[ix];
	      ix = ix + incx;
	      iy = iy + incy;
	    } // for (i = 1; i <= n; i++)

	    } // else 

	  return;
	} // dcopy
	
	private double ddot ( int n, double x[], int incx, double y[], int incy ) {

	/*****************************************************************************80
	!
	!! DDOT forms the dot product of two vectors.
	!
	!  Modified:
	!
	!    02 June 2000
	!
	!  Author:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
	!
	!  Reference:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input, integer N, the number of entries in the vectors.
	!
	!    Input, double X(*), one of the vectors to be multiplied.
	!
	!    Input, integer INCX, the increment between successive entries of X.
	!
	!    Input, double Y(*), one of the vectors to be multiplied.
	!
	!    Input, integer INCY, the increment between successive elements of Y.
	!
	!    Output, double DDOT, the dot product of X and Y.
	*/

	  int i;
	  int ix;
	  int iy;
	  double result = 0.0;

	  if ( n <= 0 ) {

	    result = 0.0;
	  }
	  else if ( incx == 1 && incy == 1 ) {
        for (i = 1; i <= n; i++) {
	        result += x[i]*y[i];
        }
	  }

	  else {

	    if ( incx >= 0 ) {
	      ix = 1;
	    }
	    else {
	      ix = ( - n + 1 ) * incx + 1;
	    }

	    if ( incy >= 0 ) {
	      iy = 1;
	    }
	    else {
	      iy = ( - n + 1 ) * incy + 1;
	    }

	    for (i = 1; i <= n; i++) {
	      result = result + x[ix] * y[iy];
	      ix = ix + incx;
	      iy = iy + incy;
	    }

	  } // else 

	  return result;
	} // ddot
	
	private void dmout ( int m, int n, int lda, double a[][], String ifmt, int idigit ) {

	/*****************************************************************************80
	!
	!! DMOUT prints double precision matrices.
	!
	!  Example:
	!
	!    PRINT AN ARRAY CALLED (SIMPLEX TABLEAU   ) OF SIZE 10 BY 20 SHOWING
	!    6 DECIMAL DIGITS PER NUMBER. THE USER IS RUNNING ON A TIME-SHARING
	!    SYSTEM WITH A 72 COLUMN OUTPUT DEVICE.
	!
	!     double precision TABLEU(20,20)
	!     M = 10
	!     N = 20
	!     LDTABL = 20
	!     IDIGIT = -6
	!     CALL DMOUT(M,N,LDTABL,TABLEU,'(''1SIMPLEX TABLEAU'')',IDIGIT)
	!
	!  Author:
	!
	!    John Wisniewski, Richard Hanson,
	!    Sandia National Laboratory.
	!
	!  Parameters:
	!
	!  M,N,LDA,A(*,*) PRINT THE double precision ARRAY A(I,J),I=1,...,M,
	!                 J=1,...,N, ON OUTPUT UNIT *. LDA IS THE DECLARED
	!                 FIRST DIMENSION OF A(*,*) AS SPECIFIED IN THE CALLING
	!                 PROGRAM. THE HEADING IN THE FORTRAN FORMAT STATEMENT
	!                 IFMT(*), DESCRIBED BELOW, IS PRINTED AS A FIRST STEP.
	!                 THE COMPONENTS A(I,J) ARE INDEXED, ON OUTPUT, IN A
	!                 PLEASANT FORMAT.
	!
	!  IFMT(*)        A FORTRAN FORMAT STATEMENT. THIS IS PRINTED ON
	!                 OUTPUT UNIT * WITH THE VARIABLE FORMAT FORTRAN
	!                 STATEMENT
	!                       write(*,IFMT).
	!
	!  IDIGIT         PRINT AT LEAST IABS(IDIGIT) DECIMAL DIGITS PER NUMBER.
	!                 THE SUBPROGRAM WILL CHOOSE THAT integer 6,14,20 OR 28
	!                 WHICH WILL PRINT AT LEAST IABS(IDIGIT) NUMBER OF
	!                 PLACES.  IF IDIGIT.LT.0, 72 PRINTING COLUMNS ARE
	!                 UTILIZED TO WRITE EACH LINE OF OUTPUT OF THE ARRAY
	!                 A(*,*). (THIS CAN BE USED ON MOST TIME-SHARING
	!                 TERMINALS).  IF IDIGIT.GE.0, 133 PRINTING COLUMNS ARE
	!                 UTILIZED. (THIS CAN BE USED ON MOST LINE PRINTERS).
	*/

	 /* //double a(lda,n)
	  int i;
	  char icol[] = new char[4];
	  int j;
	  int k1;
	  int k2;
	  int m;
	  int ndigit;
	  boolean do10 = false;
	  boolean do20 = false;
	  boolean do80 = false;
	  icol[1] = 'c';
	  icol[2] = 'o';
	  icol[3] = 'l';
	  String iStr;
	  int sLen;
	  int padLen;
	  int p;

	  Preferences.debug(ifmt + "\n");

	  if ( m <= 0 || n <= 0 || lda <= 0 ) {
	    return;
	  }

	  ndigit = idigit;
	  if ( idigit == 0) {
		  ndigit = 6;
	  }
	  if ( idigit>=0) {
		  do80 = true;
	  }
	  else {
	      ndigit = -idigit
	      if ( ndigit > 6) {
	    	  do20 = true;
	      }
	      else {
	    	  do10 = true;
	      }

	  if (do10) {
	      for (k1=1; k1 <= n; k1 += 4) {
	          k2 = Math.min(n,k1+3);
	          Preferences.debug("          ");
	          for (i = k1; i <= k2; i++) {
	        	  Preferences.debug("     col");
	        	  iStr = String.valueOf(i);
	        	  sLen = iStr.length();
	        	  padLen = 4 - sLen;
	        	  for (p = 1; p <= padLen; p++) {
	        		  Preferences.debug(" ");
	        	  }
	        	  Preferences.debug(iStr + "  ");
	        	  if (((k2 - k1) % 8) == 7) {
	        		  Preferences.debug("\n");
	        	  }
	          }
	          write(*,1000) (icol,i,i=k1,k2)
	          do i=1,m
	              write(*,1004) i,(a(i,j),j=k1,k2)
	          end do
	      } // for (k1=1; k1 <= n; k1 += 4)

	  return;
	  } // if (do10)

	   20 continue
	  if ( ndigit > 14) go to 40

	  do k1=1,n,2
	    k2 = min ( n, k1+1 )
	    write(*,1001) (icol,i,i=k1,k2)
	    do i=1,m
	      write(*,1005) i,(a(i,j),j=k1,k2)
	    end do
	  end do

	  return

	   40 continue
	  if ( ndigit > 20) go to 60

	  do k1=1,n,2
	    k2=min(n,k1+1)
	    write(*,1002) (icol,i,i=k1,k2)
	    do i=1,m
	      write(*,1006) i,(a(i,j),j=k1,k2)
	    end do
	  end do
	  return

	   60 continue
	  do 70 k1=1,n
	  k2 = k1
	  write(*,1003) (icol,i,i=k1,k2)
	  do 70 i=1,m
	  write(*,1007) i,(a(i,j),j=k1,k2)
	   70 continue
	  return

	   80 continue
	  if ( ndigit > 6) go to 100

	  do k1=1,n,8
	    k2 = min(n,k1+7)
	    write(*,1000) (icol,i,i=k1,k2)
	    do i=1,m
	      write(*,1004) i,(a(i,j),j=k1,k2)
	    end do
	  end do
	  return

	  100 continue
	  if ( ndigit > 14) go to 120

	  do k1=1,n,5
	    k2 = min(n,k1+4)
	    write(*,1001) (icol,i,i=k1,k2)
	    do i=1,m
	      write(*,1005) i,(a(i,j),j=k1,k2)
	    end do
	  end do

	  return

	  120 continue
	  if ( ndigit > 20) go to 140

	  do k1=1,n,4
	    k2 = min(n,k1+3)
	    write(*,1002) (icol,i,i=k1,k2)
	    do i=1,m
	      write(*,1006) i,(a(i,j),j=k1,k2)
	    end do
	  end do

	  return

	  140 continue

	  do k1=1,n,3
	    k2 = min(n,k1+2)
	    write(*,1003) (icol,i,i=k1,k2)
	    do i=1,m
	      write(*,1007) i,(a(i,j),j=k1,k2)
	    end do
	  end do

	  return
	 1000 format(10x,8(5x,3a1,i4,2x))
	 1001 format(10x,5(9x,3a1,i4,6x))
	 1002 format(10x,4(12x,3a1,i4,9x))
	 1003 format(10x,3(16x,3a1,i4,13x))
	 1004 format(1x,'row',i4,2x,1p8d14.5)
	 1005 format(1x,'row',i4,2x,1p5d22.13)
	 1006 format(1x,'row',i4,2x,1p4d28.19)
	 1007 format(1x,'row',i4,2x,1p3d36.27) */
    } // dmout
	
	private double dnrm2 ( int n, double x[], int incx ) {

	/*****************************************************************************80
	!
	!! DNRM2 returns the euclidean norm of a double precision vector.
	!
	!  Discussion:
	!
	!     DNRM2 ( X ) = sqrt ( X' * X )
	!
	!  Modified:
	!
	!    16 May 2005
	!
	!  Author:
	!
	!    Sven Hammarling
	!
	!    Fortran90 translation by John Burkardt.
	!
	!  Reference:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart,
	!    LINPACK User's Guide,
	!    SIAM, 1979.
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input, integer N, the number of entries in the vector.
	!
	!    Input, double X(*), the vector whose norm is to be computed.
	!
	!    Input, integer INCX, the increment between successive entries of X.
	!
	!    Output, double DNRM2, the Euclidean norm of X.
	*/

	  double absxi;
	  int ix;
	  double norm = 0.0;
	  double scale;
	  double ssq;
	  double temp;

	  if ( n < 1 || incx < 1 ) {

	    norm  = 0.0;
	  }
	  else if ( n == 1 ) {
	    norm  = Math.abs ( x[1] );
	  }
	  else {

	    scale = 0.0;
	    ssq = 1.0;

	    for (ix = 1; ix <= 1 + ( n - 1 )*incx; ix += incx) {
	      if ( x[ix] != 0.0D+00 ) {
	        absxi = Math.abs ( x[ix] );
	        if ( scale < absxi ) {
	          temp = scale/absxi;
	          ssq = 1.0 + ssq * temp * temp;
	          scale = absxi;
	        }
	        else {
	          temp = absxi/scale;
	          ssq = ssq + temp * temp;
	        }
	      } // if ( x[ix] != 0.0D+00 )
	    } // for (ix = 1; ix <= 1 + ( n - 1 )*incx; ix += incx)
	    norm  = scale * Math.sqrt ( ssq );
	  }

	  return norm;
	} // dnrm2
	
	private void drot ( int n, double x[], int incx, double y[], int incy,
			            double c, double s ) {

	/*****************************************************************************80
	!
	!! DROT applies a plane rotation.
	!
	!  Modified:
	!
	!    08 April 1999
	!
	!  Author:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
	!
	!  Reference:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input, integer N, the number of entries in the vectors.
	!
	!    Input/output, double X(*), one of the vectors to be rotated.
	!
	!    Input, integer INCX, the increment between successive entries of X.
	!
	!    Input/output, double Y(*), one of the vectors to be rotated.
	!
	!    Input, integer INCY, the increment between successive elements of Y.
	!
	!    Input, real double C, S, parameters (presumably the cosine and sine 
	!    of some angle) that define a plane rotation.
	*/

	  int i;
	  int ix;
	  int iy;
	  double stemp;

	  if ( n <= 0 ) {
		  
	  }
	  else if ( incx == 1 && incy == 1 ) {

	    for (i = 1; i <= n; i++) {
	      stemp = c * x[i] + s * y[i];
	      y[i] = c * y[i] - s * x[i];
	      x[i] = stemp;
	    } // for (i = 1; i <= n; i++)
	  } // else if ( incx == 1 && incy == 1 )
	  else {

	    if ( incx >= 0 ) {
	      ix = 1;
	    }
	    else {
	      ix = ( - n + 1 ) * incx + 1;
	    }

	    if ( incy >= 0 ) {
	      iy = 1;
	    }
	    else {
	      iy = ( - n + 1 ) * incy + 1;
	    }

	    for (i = 1; i <= n; i++) {
	      stemp = c * x[ix] + s * y[iy];
	      y[iy] = c * y[iy] - s * x[ix];
	      x[ix] = stemp;
	      ix = ix + incx;
	      iy = iy + incy;
	    } // for (i = 1; i <= n; i++)

	  }

	  return;
	} // drot
	
	private void drotg ( double sa[], double sb[], double c[], double s[] ) {

	/*****************************************************************************80
	!
	!! DROTG constructs a Givens plane rotation.
	!
	!  Modified:
	!
	!    08 April 1999
	!
	!  Author:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
	!
	!  Reference:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input/output, double SA, SB, ...
	!
	!    Output, real double C, S, ...
	*/

	  double r;
	  double roe;
	  double scale;
	  double z;
	  double temp;
	  double temp2;

	  if ( Math.abs ( sa[0] ) > Math.abs ( sb[0] ) ) {
	    roe = sa[0];
	  }
	  else {
	    roe = sb[0];
	  }

	  scale = Math.abs ( sa[0] ) + Math.abs ( sb[0] );

	  if ( scale == 0.0 ) {
	    c[0] = 1.0;
	    s[0] = 0.0;
	    r = 0.0;
	  }
	  else {
		temp = sa[0]/scale;
		temp2 = sb[0]/scale;
	    r = scale * Math.sqrt ( temp*temp + temp2*temp2 );
	    if (roe < 0) {
	    	r = -r;
	    }
	    c[0]= sa[0] / r;
	    s[0] = sb[0] / r;
	  }

	  if ( Math.abs ( c[0] ) > 0.0 && Math.abs ( c[0] ) <= s[0] ) {
	    z = 1.0 / c[0];
	  }
	  else {
	    z = s[0];
	  }

	  sa[0] = r;
	  sb[0] = z;

	  return;
    } // drotg
	
	private void dscal ( int n, double sa, double x[], int incx ) {

	/*****************************************************************************80
	!
	!! DSCAL scales a vector by a constant.
	!
	!  Modified:
	!
	!    08 April 1999
	!
	!  Author:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
	!
	!  Reference:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input, integer N, the number of entries in the vector.
	!
	!    Input, double SA, the multiplier.
	!
	!    Input/output, double X(*), the vector to be scaled.
	!
	!    Input, integer INCX, the increment between successive entries of X.
	*/

	  int i;
	  int ix;
	  int m;

	  if ( n <= 0 ) {
		  
	  }
	  else if ( incx == 1 ) {

	    m = ( n % 5 );
        for (i = 1; i <= m; i++) {
	        x[i] = sa * x[i];
        }

	    for (i = m+1; i <= n; i +=5) {
	      x[i]   = sa * x[i];
	      x[i+1] = sa * x[i+1];
	      x[i+2] = sa * x[i+2];
	      x[i+3] = sa * x[i+3];
	      x[i+4] = sa * x[i+4];
	    }
	  }

	  else {

	    if ( incx >= 0 ) {
	      ix = 1;
	    }
	    else {
	      ix = ( - n + 1 ) * incx + 1;
	    }

	    for (i = 1; i <= n; i++) {
	      x[ix] = sa * x[ix];
	      ix = ix + incx;
	    }

	  }

	  return;
	} // dscal
	
	private int idamax ( int n, double x[], int incx ) {

	/*****************************************************************************80
	!
	!! IDAMAX finds the index of the vector element of maximum absolute value.
	!
	!  Modified:
	!
	!    08 April 1999
	!
	!  Author:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
	!
	!  Reference:
	!
	!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
	!    Basic Linear Algebra Subprograms for Fortran Usage,
	!    Algorithm 539,
	!    ACM Transactions on Mathematical Software,
	!    Volume 5, Number 3, September 1979, pages 308-323.
	!
	!  Parameters:
	!
	!    Input, integer N, the number of entries in the vector.
	!
	!    Input, double X(*), the vector to be examined.
	!
	!    Input, integer INCX, the increment between successive entries of SX.
	!
	!    Output, integer IDAMAX, the index of the element of SX of maximum
	!    absolute value.
	*/

	  int i;
	  int index = 1;
	  int ix;
	  double damax;

	  if ( n <= 0 ) {
	    index = 0;
	  }
	  else if ( n == 1 ) {
	    index = 1;
	  }
	  else if ( incx == 1 ) {

	    index = 1;
	    damax = Math.abs ( x[1] );

	    for (i = 2; i <= n; i++) {
	      if ( Math.abs ( x[i] ) > damax ) {
	        index = i;
	        damax = Math.abs ( x[i] );
	      }
	    } // for (i = 2; i <= n; i++)
	  } // else if (incx == 1)
	  else {

	    if ( incx >= 0 ) {
	      ix = 1;
	    }
	    else {
	      ix = ( - n + 1 ) * incx + 1;
	    }

	    index = 1;
	    damax = Math.abs ( x[ix] );

	    ix = ix + incx;

	    for (i = 2; i <= n; i++) {
	      if ( Math.abs ( x[ix] ) > damax ) {
	        index = i;
	        damax = Math.abs ( x[ix] );
	      }
	      ix = ix + incx;
	    } // for (i = 2; i <= n; i++)

	  }

	  return index;
	} // idamax
    
    private void xerrwv (String xmess, int nerr, int level, int ni, int i1, int i2,
    		             int nr, double r1, double r2 ) {

    /*****************************************************************************80
    !
    !! XERRWV is an error output message routine.
    !
    !  Modified:
    !
    !    28 July 2006
    !
    !  Author:
    !
    !    Ron Jones
    !
    !  Reference:
    !
    !    David Kahaner, Cleve Moler, Steven Nash,
    !    Numerical Methods and Software,
    !    Prentice Hall, 1988.
    !
    !    Ron Jones, David Kahaner, 
    !    XERROR, The SLATEC Error Handling Package, 
    !    SAND82-0800, Sandia Laboratories, 1982.
    !
    !  Parameters:
    !
    !    Input, String XMESS, the message to be processed.
    !
    !    Input, integer NERR, the error number associated with this message.
    !    NERR must not be zero.
    !
    !    Input, integer LEVEL, the error category.
    !    2 means this is an unconditionally fatal error.
    !    1 means this is a recoverable error.  (i.e., it is
    !      non-fatal if xsetf has been appropriately called.)
    !    0 means this is a warning message only.
    !    -1 means this is a warning message which is to be printed at most 
    !      once, regardless of how many times this call is executed.
    !
    !    Input, integer NI, the number of integer values to be printed. (0 to 2)
    !
    !    Input, integer I1, I2, the first and second integer values.
    !
    !    Input, integer NR, the number of real values to be printed. (0 to 2)
    !
    !    Input, real ( kind = 8 ) R1, R2, the first and second real values.
    */
      
      if (!outputMes) {
    	  return;
      }

      Preferences.debug(xmess.trim() + "\n");
      Preferences.debug("error number = " + nerr + " message level = " + level + "\n");

      if ( ni == 1 || ni == 2 ){
        Preferences.debug("i1 = " + i1 + "\n");
      }

      if ( ni == 2) {
        Preferences.debug("i2 = " + i2 + "\n");
      }

      if ( nr == 1 || nr == 2) {
        Preferences.debug("r1 = " + r1 + "\n");
      }

      if ( nr == 2){
        Preferences.debug("r2 = " +  r2 + "\n");
      }

      return;
    } // xerrwv
}