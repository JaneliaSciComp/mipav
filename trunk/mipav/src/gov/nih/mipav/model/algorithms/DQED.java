package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/** This is a port of the FORTRAN90 source code dqed.f90.  DQED solves (square) systems of nonlinear equations,
    or minimizes the residual in a set of nonlinear equations, using least squares.  The user may include 
    simple bounds or linear constraints on variables.  DQED was written by Richard Hanson and Fred Krogh
    of Sandia National Laboratory. */

public abstract class DQED {
	
	private int idope[] = new int[6];
	
	private int igo_dbocls;
	
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
			  int lopt;
			  int lp;
			  int lrw;
			  int mdwl;
			  int mnew = 0;
			  int modec;
			  int mout = 0;
			  int nerr;
			  boolean pretri = false;
			  double rdum;
			  double t = 0.0;
			  double t1;
			  double t2;
			  double wt;
			  double arr[];

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
			      //call dbols(w(mcon+1,1),mdw,mout,ncols,bl,bu,ind,jopt,x,rnorm, &
			             //mode,rw(irw),iw(iiw))
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
			      //call dbols(w,mdw,mcon,ncols+mcon,bl,bu,ind,jopt,x,rnormc, &
			        //modec,rw(irw),iw(iiw))
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
			  //call dbols(w,mdw,mout+mcon,ncols+mcon,bl,bu,ind,iopt(lopt),x, &
			    //rnorm,mode,rw(lrw),iw(liw))


			  if ( 0 <= mode[0] ) {
			    mode[0] = -nerr;
			  }

			  igo_dbocls = 0;

			  return;
    } // dbocls
	
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