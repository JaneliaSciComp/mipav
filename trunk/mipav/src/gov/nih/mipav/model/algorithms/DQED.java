package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;
import java.text.DecimalFormat;

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
	
	private int iflag_dqedip = 0;
	
	
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

			  double alpha = 0.0;
			  double beta = 0.0;
			  double big = Double.MAX_VALUE;
			  //double bl(ncols)
			  double bou;
			  //double bu(ncols)
			  double cl1[] = new double[1];
			  double cl2[] = new double[1];
			  double cl3;
			  boolean cnz;
			  double colabv;
			  double colblo;
			  boolean constr;
			  double fac = 0.0;
			  boolean found = false;
			  int i = 0;
			  int icase;
			  int idum = 0;
			  int igopr = 0;
			  //int ind(ncols)
			  int ioff;
			  int ip;
			  int iprint = 0;
			  int itemp;
			  int iter = 0;
			  int itmax = 0;
			  int j;
			  int jbig = 0;
			  int jcol;
			  int jdrop;
			  int jdrop1 = 0;
			  int jdrop2 = 0;
			  int jp;
			  int lds = 0;
			  int level;
			  int lgopr = 0;
			  int lp = 0;
			  int mrows = 0;
			  int mval = 0;
			  int n;
			  int nerr = 0;
			  int nlevel;
			  int nsetb = 0;
			  double rdum = 0.0;
			  double sc[] = new double[1];
			  double ss[] = new double[1];
			  double t;
			  double t1;
			  double t2;
			  double tolind = 0.0;
			  double tolsze;
			  //double w(mdw,*)
			  double wlarge;
			  double wla = 0.0;
			  double wlb = 0.0;
			  double wt = 1.0;
			  double xnew;
			  boolean do50 = false;
			  boolean do60 = false;
			  boolean do70 = false;
			  boolean do90 = false;
			  boolean do100 = false;
			  boolean do130 = false;
			  boolean do180 = false;
			  boolean do200 = false;
			  boolean do230 = false;
			  boolean do240 = false;
			  boolean do260 = false;
			  boolean do290 = false;
			  boolean do310 = false;
			  boolean do330 = false;
			  boolean do340 = false;
			  boolean do390 = false;
			  boolean do420 = false;
			  boolean do430 = false;
			  boolean do460 = false;
			  boolean do570 = false;
			  boolean do580 = false;
			  double arr[];
			  double arr2[];
			  double temp;
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
				  do50 = false;
				  do60 = true;
				  if ( iprint > 0) {
				      dmout(mrows,ncols+1,mdw,w,"pretri. input matrix",-4);
				      dvout(ncols,bl,"lower bounds",-4);
				      dvout(ncols,bu,"upper bounds",-4);
				  }
			  } // if (do50)

			  if (do60) {
		          do60 = false;
			      iter = iter + 1;
			      if ( iter<=itmax) {
				      do180 = true;
			      }
			      else {
			          nerr = 22;
			          xerrwv("dbolsm(). more than (i1)=itmax iterations solving bounded least squares problem.",
			                  nerr,level,1,itmax,idum,0,rdum,rdum);
			          //
			          // RESCALE AND TRANSLATE VARIABLES
			          //
			          igopr = 1;
			          do130 = true;
			       } // else
			  } // if (do60)
	  

			   if (do70) {
			       mode[0] = -nerr;
			       return;
			   } // if (do70)

			   if (do90) {
                   do90 = false;
			       if ( found ) {
			    	   //
					   //  SOLVE THE TRIANGULAR SYSTEM
					   //
					   lgopr = 1;
					   do260 = true;
			       }
			       else {
			           //
			           // RESCALE AND TRANSLATE VARIABLES
			           // 
			           igopr = 2;
			           do130 = true;
			       } // else   
			   } // if (do90)

			  if (do100) {
			      mode[0] = nsetb;
			      return;
			  }

			  //
			  //  RESCALE AND TRANSLATE VARIABLES
			  //
			  if (do130) {
                  do130 = false;
                  for (j = 1; j <= nsetb; j++) {
                	  rw[j] = x[j];
                  }
                  
                  for (j = 1; j <= ncols; j++) {
                	  x[j] = 0.0;
                  }

			      for (j = 1; j <= nsetb; j++) {
			          jcol = Math.abs(ibasis[j]);
			          x[jcol] = rw[j]*Math.abs(scl[jcol]);
			      } // for (j = 1; j <= nsetb; j++)

			      for (j = 1; j <= ncols; j++) {
			          if ((ibb[j] % 2) == 0) {
			        	  x[j] = bu[j] - x[j];
			          }
			      } // for (j = 1; j <= ncols; j++)

			      for (j = 1; j <= ncols; j++) {
			          jcol = ibasis[j];
			          if ( jcol < 0) {
			        	  x[-jcol] = bl[-jcol] + x[-jcol];
			          }
			      } // for (j = 1; j <= ncols; j++)

			      for (j = 1; j <= ncols; j++) {
			          if ( scl[j] < 0.0 ) {
			        	  x[j] = -x[j];
			          }
			      } // (j = 1; j <= ncols; j++)

			      arr = new double[mrows-mval+1];
			      for (j = 1; j <= mrows - mval; j++) {
			    	  arr[j] = w[Math.min(mval+1,mrows)+j-1][ncols+1];
			      }
			      dscal ( mrows-mval, wt, arr, 1 );
			      for (j = 1; j <= mrows - mval; j++) {
			    	  w[Math.min(mval+1,mrows)+j-1][ncols+1] = arr[j];
			      }

			      arr = new double[mrows - Math.max(nsetb,mval) + 1];
			      for (j = 1; j <= mrows - Math.max(nsetb,mval); j++) {
			    	  arr[j] = w[Math.min((Math.max(nsetb,mval) + 1),mrows) + j - 1][ncols+1];
			      }
			      rnorm[0] = dnrm2(mrows-Math.max(nsetb,mval),arr,1);
                  if (igopr == 1) {
                	  do70 = true;
                  }
                  else if (igopr == 2) {
                	  do100 = true;
                  }
                  else {
                	  do180 = true;
                  }
			  } // if (do130)
			  //
			  //  FIND A VARIABLE TO BECOME NON-ACTIVE
			  //
			  if (do180) {
		          do180 = false;
		          do200 = true;
			      //
			      //  COMPUTE (NEGATIVE) OF GRADIENT VECTOR, W=(TRANSPOSE OF E)*(F-E*X).
			      //
		          for (j = 1; j <= ncols; j++) {
		        	  ww[j] = 0.0;
		          }

			      for (j = nsetb + 1; j <= ncols; j++) {
			          jcol = Math.abs ( ibasis[j] );
			          arr = new double[mrows-nsetb+1];
			          arr2 = new double[mrows-nsetb+1];
			          for (n = 1; n <= mrows - nsetb; n++) {
			        	  arr[j] = w[Math.min(nsetb+1, mrows) + n - 1][j];
			        	  arr2[j] = w[Math.min(nsetb+1,mrows) + n - 1][ncols+1];
			          }
			          ww[j] = ddot(mrows-nsetb,arr,1,arr2,1)*Math.abs(scl[jcol]);
			      } // for (j = nsetb + 1; j <= ncols; j++)

			      if ( iprint > 0) {
			          dvout(ncols,ww,"Gradient values",-4);
			          ivout(ncols,ibasis,"internal variable order",-4);
			          ivout(ncols,ibb,"bound polarity",-4);
			      } // if (iprint > 0)
			  } // if (do180)

			  if (do200) {
				  do200 = false;
			      //
			      //  IF ACTIVE SET = NUMBER OF TOTAL ROWS, QUIT.
			      //
			  if ( nsetb == mrows) {
			      found = false;
			      do90 = true;
			      continue loop;
			  }
			  //
			  //  CHOOSE AN EXTREMAL COMPONENT OF GRADIENT VECTOR
			  //  FOR A CANDIDATE TO BECOME NON-ACTIVE.
			  //
			  wlarge = -big;
			  jbig = 0;
			  cnz = false;

			  forLoop: for (j = nsetb + 1; j <= ncols; j++) {

			    t = ww[j];
			    //
			    // SKIP LOOKING AT COMPONENTS FLAGGED AS NON-CANDIDATES.
			    //
			    if ( t == big) {
			      continue forLoop;
			    }

			    itemp = ibasis[j];
			    jcol = Math.abs(itemp);
			    if ( nsetb < mval) {
			    	 arr = new double[nsetb+1];
			    	 for (n = 1; n <= nsetb; n++) {
			    		 arr[n] = w[n][j];
			    	 }
			         cl1[0] = dnrm2(nsetb,arr,1);
			         arr = new double[mval-nsetb+1];
			         for (n = 1; n <= mval-nsetb; n++) {
			        	 arr[n] = w[Math.min(nsetb+1,mrows)+n-1][j];
			         }
			         cl2[0] = dnrm2(mval-nsetb,arr,1);
			         colabv = cl1[0];
			         colblo = cl2[0];
			    } // if
			    else {
			    	 arr = new double[mval+1];
			    	 for (n = 1; n <= mval; n++) {
			    		 arr[n] = w[n][j];
			    	 }
			         cl1[0] = dnrm2(mval,arr,1);
			         arr = new double[nsetb-mval+1];
			         for (n = 1; n <= nsetb-mval; n++) {
			        	 arr[n] = w[Math.min(mval+1,mrows)+n-1][j];
			         }
			         cl2[0] = Math.abs(wt)*dnrm2(nsetb-mval,arr,1);
			         arr = new double[mrows-nsetb+1];
			         for (n = 1; n <= mrows-nsetb; n++) {
			        	 arr[n] = w[Math.min(nsetb+1, mrows)+n-1][j];
			         }
			         cl3 = Math.abs(wt)*dnrm2(mrows-nsetb,arr,1);
			         drotg(cl1,cl2,sc,ss);
			         colabv = Math.abs(cl1[0]);
			         colblo = cl3;
			    } // else

			    if ( itemp < 0) {
			      if ((ibb[jcol] % 2) == 0) {
			    	  t = -t;
			      }
			    
			    //
			    //  SKIP LOOKING AT COMPONENTS THAT WOULD NOT DECREASE OBJECTIVE.
			    //
			      if ( t < 0.0 ) {
			        continue forLoop;
			      }

			    } // if (itemp < 0)
			//
			//  THIS IS A COLUMN PIVOTING STEP THAT MAXIMIZES THE RATIO OF
			//  COLUMN MASS ON AND BELOW THE PIVOT LINE RELATIVE TO THAT
			//  STRICTLY ABOVE THE PIVOT LINE.
			//
			     if ( (colabv == 0.0) && (!cnz)) {
			         t = colblo*Math.abs(scl[jcol]);
			         if ( wlarge < t) {
			             wlarge = t;
			             jbig = j;
			         } // if (wlarge < t)
			     } // if ( (colabv == 0.0) && (!cnz))
			     else {
			         if (!cnz) {
			             wla = 0.0;
			             wlb = 0.0;
			             cnz = true;
			         } // if (!cnz)

			       if ( Math.sqrt(colblo)*Math.sqrt(wla) >= Math.sqrt(colabv)*Math.sqrt(wlb)) {
			            wlb=colblo;
			            wla=colabv;
			            jbig=j;
			       } 

			     } // else

			  } // for (j = nsetb + 1; j <= ncols; j++)

			  if ( jbig == 0) {
			      found = false;
			      if ( iprint > 0) {
			        Preferences.debug("Found no variable to enter.\n");
			      }

			      do90 = true;
			      continue loop;

			  } // if (jbig == 0)
			  //
			  //  SEE IF THE INCOMING COL. IS SUFFICIENTLY INDEPENDENT.
			  //  THIS TEST IS MADE BEFORE AN ELIMINATION IS PERFORMED.
			  //
			  if ( iprint > 0) {
			    Preferences.debug("Try to bring in column " +  jbig + "\n");
			  }

			  if ( cnz) {

			    if ( wlb<=wla*tolind) {
			      found = false;
			      if ( iprint > 0) {
			        Preferences.debug("Variable is dependent, not used.\n");
			      }

			      ww[jbig] = big;
			      do200 = true;
			      continue loop;

			    } // if ( wlb<=wla*tolind)

			  } // if (cnz)
			  //
			  //  SWAP MATRIX COLS. NSETB+1 AND JBIG, PLUS POINTER INFO., AND
			  //  GRADIENT VALUES.
			  //
			  nsetb = nsetb + 1;
			  if ( nsetb != jbig) {
				  for (n = 1; n <= mrows; n++) {
			          temp = w[n][nsetb];
			          w[n][nsetb] = w[n][jbig];
			          w[n][jbig] = temp;
				  }
			      temp = ww[nsetb];
			      ww[nsetb] = ww[jbig];
			      ww[jbig] = temp;
			      itemp = ibasis[nsetb];
			      ibasis[nsetb] = ibasis[jbig];
			      ibasis[jbig] = itemp;
			  } // if (nsetb != jbig)
			  //
			  //  ELIMINATE ENTRIES BELOW THE PIVOT LINE IN COL. NSETB.
			  //
			  if ( mrows > nsetb) {

			      for (i = mrows; i >= nsetb + 1; i--) {
			         if ( i != mval+1 ) {
			           arr = new double[1];
			           arr[0] = w[i-1][nsetb];
			           arr2 = new double[1];
			           arr2[0] = w[i][nsetb];
			           drotg(arr,arr2,sc,ss);
			           w[i-1][nsetb] = arr[0];
			           w[i][nsetb] = 0.0;
			           arr = new double[ncols-nsetb+2];
			           arr2 = new double[ncols-nsetb+2];
			           for (n = 1; n <= ncols-nsetb+1; n++) {
			        	   arr[n] = w[i-1][nsetb + n];
			        	   arr2[n] = w[i][nsetb+n];
			           }
			           drot(ncols-nsetb+1,arr,1,arr2,1,sc[0],ss[0]);
			           for (n = 1; n <= ncols-nsetb+1; n++) {
			        	   w[i-1][nsetb + n] = arr[n];
			        	   w[i][nsetb+n] = arr2[n];;
			           }
			         } // (i != mval+1)
			      } // for (i = mrows; i >= nsetb + 1; i--)

			      if ( (mval>=nsetb) && (mval < mrows)) {
			          t = w[nsetb][nsetb];
			          if ( t != 0.0 ) {
			              t = wt*w[mval+1][nsetb]/t;
			          }
			          else {
			              t = big;
			          }

			          if ( tolind*Math.abs(t) > 1.0) {
			        	  for (n = 1; n <= ncols-nsetb+2; n++) {
			        		  temp = w[nsetb][nsetb+n-1];
			        		  w[nsetb][nsetb+n-1] = w[mval+1][nsetb+n-1];
			        		  w[mval+1][nsetb+n-1] = temp;
			        	  }
			        	  for (n = 1; n <= ncols-nsetb+2; n++) {
			        		  w[nsetb][nsetb+n-1] = wt * w[nsetb][nsetb+n-1];
			        	  }
			              for (n = 1; n <= ncols-nsetb+2; n++) {
			            	  w[mval+1][nsetb+n-1] = 1.0/wt * w[mval+1][nsetb+n-1];
			              }
			          } // if ( tolind*Math.abs(t) > 1.0)

			          arr = new double[1];
			          arr[0] = w[nsetb][nsetb];
			          arr2 = new double[1];
			          arr2[0] = w[mval+1][nsetb];
			          drotg(arr,arr2,sc,ss);
			          w[nsetb][nsetb] = arr[0];
			          w[mval+1][nsetb] = 0.0;
			          arr = new double[ncols-nsetb+2];
			          arr2 = new double[ncols-nsetb+2];
			          for (n = 1; n <= ncols-nsetb+1; n++) {
			        	  arr[n] = w[nsetb][nsetb+n];
			        	  arr2[n] = w[mval+1][nsetb+n];
			          }
			          drot(ncols-nsetb+1,arr,1,arr2,1,sc[0],ss[0]);
			          for (n = 1; n <= ncols-nsetb+1; n++) {
			        	  w[nsetb][nsetb+n] = arr[n];
			        	  w[mval+1][nsetb+n] = arr2[n];
			          }
			      } // if ( (mval>=nsetb) && (mval < mrows))

			  } // if (mrows > nsetb)

			  if ( w[nsetb][nsetb] == 0.0 ) {
			      ww[nsetb] = big;
			      nsetb = nsetb - 1;
			      if ( iprint > 0) {
			        Preferences.debug("Pivot is zero, not used.\n");
			      }

			      do200 = true;
			      continue loop;
			  } // if ( w[nsetb][nsetb] == 0.0 )
			  //
			  //  CHECK THAT NEW VARIABLE IS MOVING IN THE RIGHT DIRECTION.
			  //
			  itemp = ibasis[nsetb];
			  jcol = Math.abs(itemp);
			  xnew = (w[nsetb][ncols+1]/w[nsetb][nsetb])/Math.abs(scl[jcol]);

			  if ( itemp < 0) {
			      if ( ww[nsetb]>= 0.0 && xnew<= 0.0 ) {
			    	  do230 = true;
			      }
			      else if ( ww[nsetb]<= 0.0 && xnew>= 0.0 ) {
			    	  do230 = true;
			      }
			      else {
			    	  do240 = true;
			      }
			  } // if (itemp < 0)
			  else {
                  do240 = true;
			  } // else
			  } // if (do200)

			  if (do230) {
				  do230 = false;
			      ww[nsetb] = big;
			      nsetb = nsetb - 1;
			      if ( iprint > 0) {
			          Preferences.debug("Variable has bad direction, not used.\n");
			      }

			      do200 = true;
			      continue loop;
			  } // if (do230)

			  if (do240) {
				  do240 = false;
			      found = true;
			      do90 = true;
			      continue loop;
			  } // if (do240)

			  //
			  //  SOLVE THE TRIANGULAR SYSTEM
			  //
			  if (do260) {
		          do260 = false;
		          for (j = 1; j <= nsetb; j++) {
		        	  rw[j] = w[j][ncols+1];
		          }

			     for (j = nsetb; j >= 1; j--) {
			         rw[j] = rw[j]/w[j][j];
			         jcol = Math.abs(ibasis[j]);
			         t = rw[j];
			         if ((ibb[jcol] % 2) == 0) {
			        	 rw[j] = -rw[j];
			         }
			         arr = new double[j];
			         for (n = 1; n <= j; n++) {
			        	 arr[n] = w[n][j];
			         }
			         daxpy(j-1,-t,arr,1,rw,1);
			         rw[j] = rw[j]/Math.abs(scl[jcol]);
			     } // for (j = nsetb; j >= 1; j--)

			     if ( iprint > 0) {
			         dvout(nsetb,rw,"soln. values",-4);
			         ivout(nsetb,ibasis,"cols. used",-4);
			     } // if (iprint > 0)

			     if (lgopr == 1) {
			    	 do290 = true;
			     }
			     else if (lgopr == 2) {
			    	 do430 = true;
			     }
			     else {
			    	  //
				      //  SOLVE THE TRIANGULAR SYSTEM
				      //
				      lgopr = 1;
				      do260 = true;
				      continue loop;
			     }
			  } // if (do260)

			  if (do290) {
				  do290 = false;
			      //
			      //  SEE IF THE UNCONSTRAINED SOL. (OBTAINED BY SOLVING THE
			      //  TRIANGULAR SYSTEM) SATISFIES THE PROBLEM BOUNDS.
			      //
			      alpha = 2.0;
			      beta = 2.0;
			      x[nsetb] = 0.0;

			      for (j = 1; j <= nsetb; j++) {

			          itemp = ibasis[j];
			          jcol = Math.abs(itemp);
			          t1 = 2.0;
			          t2 = 2.0;

			          if ( itemp < 0) {
			              bou = 0.0;
			          }
			          else {
			              bou = bl[jcol];
			          } // else

			          if ( (-bou) != big) {
			        	  bou = bou/Math.abs(scl[jcol]);
			          }
			          if ( rw[j]<=bou) {
			        	  t1 = (x[j]-bou)/ (x[j]-rw[j]);
			          }
			          bou = bu[jcol];
			          if ( bou != big){
			        	  bou = bou/Math.abs(scl[jcol]);
			          }
			          if ( rw[j]>=bou) {
			        	  t2 = (bou-x[j])/ (rw[j]-x[j]);
			          }
			          //
			          // IF NOT, THEN COMPUTE A STEP LENGTH SO THAT THE
			          // VARIABLES REMAIN FEASIBLE.
			          //
			          if ( t1 < alpha) {
			              alpha = t1;
			              jdrop1 = j;
			          } // if (t1 < alpha)

			          if ( t2 < beta) {
			              beta = t2;
			              jdrop2 = j;
			          } // if (t2 < beta)

			      } // for (j = 1; j <= nsetb; j++)

			      constr = (alpha < 2.0) || (beta < 2.0);
			      if ( constr) {
			    	  do310 = true;
			      }
			      else {
			          //
			          // ACCEPT THE CANDIDATE BECAUSE IT SATISFIES THE STATED BOUNDS
			          // ON THE VARIABLES.
			          //
			    	  for (j = 1; j <= nsetb; j++) {
			              x[j] = rw[j];
			    	  }

			          do60 = true;
			          continue loop;
			      } // else 
			  } // if (do290)

			  if (do310) {
				  do310 = false;
				  do330 = true;
			      //
			      //  TAKE A STEP THAT IS AS LARGE AS POSSIBLE WITH ALL
			      //  VARIABLES REMAINING FEASIBLE.
			      //
			      for (j = 1; j <= nsetb; j++) {
			          x[j] = x[j] + Math.min(alpha,beta)* (rw[j]-x[j]);
			      } // for (j = 1; j <= nsetb; j++)

			      if ( alpha<=beta) {
			          jdrop2 = 0;
			      }
			      else {
			          jdrop1 = 0;
			      }
			  } // if (do310)

			  if (do330) {
				  do330 = false;
			      if ( jdrop1+jdrop2 > 0 && nsetb > 0) {
			    	  do340 = true;
			      }
			      else {
			    	  do60 = true;
			    	  continue loop;
			      }
			  } // if (do330)

			  if (do340) {
                  do340 = false;
			      jdrop = jdrop1 + jdrop2;
			      itemp = ibasis[jdrop];
			      jcol = Math.abs(itemp);
			      if ( jdrop2 > 0) {
			          //
			          //  VARIABLE IS AT AN UPPER BOUND.  SUBTRACT MULTIPLE OF THIS COL.
			          //  FROM RIGHT HAND SIDE.
			          //
			          t = bu[jcol];
			          if ( itemp > 0) {

			              bu[jcol] = t - bl[jcol];
			              bl[jcol] = -t;
			              itemp = -itemp;
			              scl[jcol] = -scl[jcol];
			              for (j = 1; j <= jdrop; j++) {
			                  w[j][jdrop] = -w[j][jdrop];
			              }
			          } // if (itemp > 0)
			          else {
			              ibb[jcol] = ibb[jcol] + 1;
			              if ((ibb[jcol] % 2) == 0) {
			            	  t = -t;
			              }
			          } // else
			          
			      } // if (jdrop2 > 0)
			      else {
			    	  //
			          //  VARIABLE IS AT A LOWER BOUND.
			          // 
			          if ( itemp < 0.0 ) {
			              t = 0.0;
			          }
			          else {
			              t = -bl[jcol];
			              bu[jcol] = bu[jcol] + t;
			              itemp = -itemp;
			          } // else

			      } // else

			      arr = new double[jdrop+1];
			      arr2 = new double[jdrop+1];
			      for (j = 1; j <= jdrop; j++) {
			    	  arr[j] = w[j][jdrop];
			    	  arr2[j] = w[j][ncols+1];
			      }
			      daxpy(jdrop,t,arr,1,arr2,1);
			      for (j = 1; j <= jdrop; j++) {
			    	  w[j][ncols+1] = arr2[j];
			      }
			      // 
			      //  MOVE CERTAIN COLS. LEFT TO ACHIEVE UPPER HESSENBERG FORM.
			      // 
			      for (j = 1; j <= jdrop; j++) {
			    	  rw[j] = w[j][jdrop];
			      }

			      for (j = jdrop + 1; j <= nsetb; j++) {
			          ibasis[j-1] = ibasis[j];
			          x[j-1] = x[j];
			          for (n = 1; n <= j; n++) {
			        	  w[n][j-1] = w[n][j];
			          }
			      } // for (j = jdrop + 1; j <= nsetb; j++)

			      ibasis[nsetb] = itemp;
			      w[1][nsetb] = 0.0;
		          for (j = jdrop+1; j <= mrows; j++) {
		        	  w[j][nsetb] = 0.0;
		          }

			      for (j = 1; j <= jdrop; j++) {
			    	  w[j][nsetb] = rw[j];
			      }
			      //
			      //  TRANSFORM THE MATRIX FROM UPPER HESSENBERG FORM TO
			      //  UPPER TRIANGULAR FORM.
			      //
			      nsetb = nsetb - 1;

			      f2loop: for (i = jdrop; i <= nsetb; i++) {
			          //
			          //  LOOK FOR SMALL PIVOTS AND AVOID MIXING WEIGHTED AND NONWEIGHTED ROWS.
			          // 
			          if ( i == mval){
			              t = 0.0;
			              for (j = i; j <= nsetb; j++) {
			                  jcol = Math.abs(ibasis[j]);
			                  t1 = Math.abs(w[i][j]*scl[jcol]);
			                  if ( t1 > t) {
			                      jbig = j;
			                      t = t1;
			                  } // if (t1 > t)
			              } // for (j = i; j <= nsetb; j++)
			              do390 = true;
			              break f2loop;
			          } // if (i == mval)
                      arr = new double[1];
                      arr[0] = w[i][i];
                      arr2 = new double[1];
                      arr2[0] = w[i+1][i];
			          drotg(arr,arr2,sc,ss);
			          w[i][i] = arr[0];
			          w[i+1][i] = 0.0;
			          arr = new double[ncols-i+2];
			          arr2 = new double[ncols-i+2];
			          for (j = 1; j <= ncols-i+1; j++) {
			        	  arr[j] = w[i][i+j];
			        	  arr2[j] = w[i+1][i+j];
			          }
			          drot(ncols-i+1,arr,1,arr2,1,sc[0],ss[0]);
			          for (j = 1; j <= ncols-i+1; j++) {
			        	  w[i][i+j] = arr[j];
			        	  w[i+1][i+j] = arr2[j];
			          }
			      } // for (i = jdrop; i <= nsetb; i++)
                  if (!do390) {
                	  do420 = true;
                  }
			  } // if (do340)

			  if (do390) {
				  do390 = false;
				  do420 = true;
			      //
			      //  THE TRIANGULARIZATION IS COMPLETED BY GIVING UP
			      //  THE HESSENBERG FORM AND TRIANGULARIZING A RECTANGULAR MATRIX.
			      //
				  for (j = 1; j <= mrows; j++) {
					  temp = w[j][i];
					  w[j][i] = w[j][jbig];
					  w[j][jbig] = temp;
				  }
				  temp = ww[i];
				  ww[i] = ww[jbig];
				  ww[jbig] = temp;
	              temp = x[i];
	              x[i] = x[jbig];
	              x[jbig] = temp;
			      itemp = ibasis[i];
			      ibasis[i] = ibasis[jbig];
			      ibasis[jbig] = itemp;
			      jbig = i;
			      for (j = jbig; j <= nsetb; j++) {
			          for (i = j + 1; i <= mrows; i++) {
			        	  arr = new double[1];
			        	  arr[0] = w[j][j];
			        	  arr2 = new double[1];
			        	  arr2[0] = w[i][j];
			              drotg(arr,arr2,sc,ss);
			              w[j][j] = arr[0];
			              w[i][j] = 0.0;
			              arr = new double[ncols-j+2];
			              arr2 = new double[ncols-j+2];
			              for (n = 1; n <= ncols-j+1; n++) {
			            	  arr[n] = w[j][j+n];
			            	  arr2[n] = w[i][j+n];
			              }
			              drot(ncols-j+1,arr,1,arr2,1,sc[0],ss[0]);
			              for (n = 1; n <= ncols-j+1; n++) {
			            	  w[j][j+n] = arr[n];
			            	  w[i][j+n] = arr2[n];
			              }
			          } // for (i = j + 1; i <= mrows; i++) 
			      } // for (j = jbig; j <= nsetb; j++)
			  } // if (do390)

			  if (do420) {
				  do420 = false;
			      //
			      //  SEE IF THE REMAINING COEFFICIENTS ARE FEASIBLE.  THEY SHOULD
			      //  BE BECAUSE OF THE WAY MIN(ALPHA,BETA) WAS CHOSEN.  ANY THAT ARE
			      //  NOT FEASIBLE WILL BE SET TO THEIR BOUNDS AND
			      //  APPROPRIATELY TRANSLATED.
			      // 
			      jdrop1 = 0;
			      jdrop2 = 0;
			      //
			      // SOLVE THE TRIANGULAR SYSTEM
			      // 
			      lgopr = 2;
			      do260 = true;
			      continue loop;
			  } // if (do420)

			  if (do430) {
				  do430 = false;
				  for (j = 1; j <= nsetb; j++) {
					  x[j] = rw[j];
				  }

			      f3loop: for (j = 1; j <= nsetb; j++) {

			          itemp = ibasis[j];
			          jcol = Math.abs(itemp);

			          if ( itemp < 0) {
			              bou = 0.0;
			          }
			          else {
			              bou = bl[jcol];
			          }

			          if ( (-bou) != big) {
			    	      bou = bou/Math.abs(scl[jcol]);
			          }

			          if ( x[j] <=bou){
			              jdrop1 = j;
			              break f3loop;
			          } // if ( x[j] <=bou)

			          bou = bu[jcol];

			          if ( bou !=big ) {
			        	  bou = bou/Math.abs(scl[jcol]);
			          }

			          if ( x[j]>=bou) {
			              jdrop2 = j;
			              break f3loop;
			          } // if ( x[j]>=bou)

			      } // for (j = 1; j <= nsetb; j++)

			      do330 = true;
			      continue loop;
			  } // if (do430)

			  //
			  // INITIALIZE VARIABLES AND DATA VALUES
			  // 
			  if (do460) {
			      do460 = false;
			      //
			      //  PRETRIANGULARIZE RECTANGULAR ARRAYS OF CERTAIN SIZES
			      //  FOR INCREASED EFFICIENCY.
			      // 
			      if ( fac*minput > ncols) {
			          for (j = 1; j <= ncols + 1; j++) {
			              for (i = minput; i >= j + mval + 1; i--) {
			            	  arr = new double[1];
			            	  arr[0] = w[i-1][j];
			            	  arr2 = new double[1];
			            	  arr2[0] = w[i][j];
			                  drotg(arr,arr2,sc,ss);
			                  w[i-1][j] = arr[0];
			                  w[i][j] = 0.0;
			                  arr = new double[ncols-j+2];
			                  arr2 = new double[ncols-j+2];
			                  for (n = 1; n <= ncols-j+1; n++) {
			                	  arr[n] = w[i-1][j+n];
			                	  arr2[n] = w[i][j+n];
			                  }
			                  drot(ncols-j+1,arr,1,arr2,1,sc[0],ss[0]);
			                  for (n = 1; n <= ncols-j+1; n++) {
			                	  w[i-1][j+n] = arr[n];
			                	  w[i][j+n] = arr2[n];
			                  }
			              } // for (i = minput; i >= j + mval + 1; i--)
			          } // for (j = 1; j <= ncols + 1; j++)
			          mrows = ncols + mval + 1;
			      } // if (fac*minput > ncols)
			      else {
			          mrows = minput;
			      }
			      //
			      //  SET THE X(*) ARRAY TO ZERO SO ALL COMPONENTS ARE DEFINED.
			      //
			      for (j = 1; j <= ncols; j++) {
			          x[j] = 0.0;
			      }
			      //
			      //  THE ARRAYS IBASIS(*), IBB(*) ARE INITIALIZED BY THE CALLING
			      //  PROGRAM UNIT.
			      //  THE COL. SCALING IS DEFINED IN THE CALLING PROGRAM UNIT.
			      //  'BIG' IS PLUS INFINITY ON THIS MACHINE.
			      //
			  big = Double.MAX_VALUE;

			  for (j = 1; j <= ncols; j++) {

			    if ( ind[j] == 1 ) {
			      bu[j] = big;
			    }
			    else if ( ind[j] == 2 ) {
			      bl[j] = -big;
			    }
			    else if ( ind[j] == 3 ) {
			    	
			    }
			    else if ( ind[j] == 4 ) {
			      bl[j] = -big;
			      bu[j] = big;
			    }

			  } // for (j = 1; j <= ncols; j++)

			  for (j = 1; j <= ncols; j++) {

			     if ( (bl[j]<= 0.0 && 0.0 <= bu[j] &&
			        Math.abs(bu[j]) < Math.abs(bl[j])) || bu[j] < 0.0 ) {
			         t = bu[j];
			         bu[j] = -bl[j];
			         bl[j] = -t;
			         scl[j] = -scl[j];
			         for (n = 1; n <= mrows; n++) {
			             w[n][j] = -w[n][j];
			         }
			     }
			     //
			     //  INDICES IN SET T(=TIGHT) ARE DENOTED BY NEGATIVE VALUES OF IBASIS(*).
			     //
			     if ( bl[j]>=0.0 ) {
			         ibasis[j] = -ibasis[j];
			         t = -bl[j];
			         bu[j] = bu[j] + t;
			         arr = new double[mrows+1];
			         arr2 = new double[mrows+1];
			         for (n = 1; n <= mrows; n++) {
			        	 arr[n] = w[n][j];
			        	 arr2[n] = w[n][ncols+1];
			         }
			         daxpy(mrows,t,arr,1,arr2,1);
			         for (n = 1; n <= mrows; n++) {
			        	 w[n][ncols+1] = arr2[n];
			         }
			     }

			  } // for (j = 1; j <= ncols; j++)

			  nsetb = 0;
			  iter = 0;

			  do50 = true;
			  continue loop;
			  } // if (do460)
			  //
			  //  PROCESS OPTION ARRAY
			  //
			  if (do570) {
                  do570 = false;
                  do580 = true;
			      if ( idope[5] == 1) {
			          fac = x[ncols+idope[1]];
			          wt = x[ncols+idope[2]];
			          mval = idope[3];
			      } // if (idope[5] == 1)
			      else {
			          fac = 0.0;
			          wt = 1.0;
			          mval = 0;
			      }

			      tolind = Math.sqrt( epsilon);
			      tolsze = Math.sqrt( epsilon);
			      itmax = 5 * Math.max ( minput, ncols );
			      iprint = 0;
			      //
			      //  CHANGES TO SOME PARAMETERS CAN OCCUR THROUGH THE OPTION
			      //  ARRAY, IOPT(*).  PROCESS THIS ARRAY LOOKING CAREFULLY
			      //  FOR INPUT DATA ERRORS.
			      //
			      lp = 0;
			      lds = 0;
			  } // if (do570)

			  if (do580) {
                  do580 = false;
			      lp = lp + lds;
			      //
			      //  TEST FOR NO MORE OPTIONS.
			      //
			      ip = iopt[lp+1];
			      jp = Math.abs(ip);
			      if ( ip == 99) {
			          do460 = true;
			          continue loop;
			      }
			      else if ( jp == 99) {
			          lds = 1;
			          do580 = true;
			          continue loop;
			      }
			      else if ( jp == 1) {
			          //
			          //  MOVE THE IOPT(*) PROCESSING POINTER.
			          //
			          if ( ip > 0) {
			              lp = iopt[lp+2] - 1;
			              lds = 0;
			          }
			          else {
			              lds = 2;
			          }

			          do580 = true;
			          continue loop;
			      } // else if (jp == 1)
			      else if ( jp == 2) {
			          //
			          //  CHANGE TOLERANCE FOR RANK DETERMINATION.
			          //
			          if (ip > 0) {
			              ioff = iopt[lp+2];
			              if ( ioff<=0) {
			                  nerr = 24;
			                  xerrwv("dbolsm(). the offset=(i1) beyond postion\nncols=(i2) must be positive for option number 2.",
			                         nerr,level,2,ioff,ncols,0,rdum,rdum);
			                  mode[0] = -nerr;
			                  return;
			              } // if (ioff <= 0)

			              tolind = x[ncols+ioff];
			              if (tolind < epsilon) {
			                  nerr = 25;
			                  nlevel = 0;
			                  xerrwv("dbolsm(). the tolerance for rank\ndetermination=(r1) is less than machine precision=(r2).",
			                         nerr,nlevel,0,idum,idum,2,tolind, epsilon);
			              } // if (tolind < epsilon)
			          } // if (ip > 0)

			          lds = 2;
			          do580 = true;
			          continue loop;
			      } // else if (jp == 2)
			      else if ( jp == 3) {
			          //
			          //  CHANGE BLOWUP FACTOR FOR ALLOWING VARIABLES TO BECOME INACTIVE.
			          //
			          if ( ip > 0) {
			              ioff = iopt[lp+2];
			              if ( ioff<=0) {
			                  nerr = 26;
			                  xerrwv("dbolsm(). the offset=(i1) beyond position\nncols=(i2) must be postive for option number 3.",
			                         nerr,level,2,ioff,ncols,0,rdum,rdum);
			                  mode[0] = -nerr;
					          return;
			              } // if (ioff <= 0)

			              tolsze = x[ncols+ioff];
			              if ( tolsze<= 0.0 ) {
			                  nerr = 27;
			                  xerrwv("dbolsm(). the reciprocal of the blow-up factor\nfor rejecting variables must be positive. now=(r1).",
			                         nerr,level,0,idum,idum,1,tolsze,rdum);
			                  mode[0] = -nerr;
					          return;
			              } // if (tolsze <= 0.0)
			          } // if (ip > 0)

			          lds = 2;
			          do580 = true;
			          continue loop;
			      } // else if (jp == 3)
			      else if ( jp == 4) {
			          //
			          //  Change the maximum number of iterations allowed.
			          //
			          if ( ip > 0) {
			              itmax = iopt[lp+2];
			              if ( itmax<=0) {
			                  nerr = 28;
			                  xerrwv("dbolsm(). the maximum number of iterations=(i1) must be positive.",
			                		  nerr,level,1,itmax,idum,0,rdum,rdum);
			                  mode[0] = -nerr;
					          return;
			              } // if (itmax <= 0)
			          } // if (ip > 0)

			          lds = 2;
			          do580 = true;
			          continue loop;
			      } // else if (jp == 4)
			      else if ( jp == 5) {
			          //
			          //  CHANGE THE FACTOR FOR PRETRIANGULARIZING THE DATA MATRIX.
			          //
			          if ( ip > 0) {
			              ioff = iopt[lp+2];
			              if ( ioff<=0) {
			                  nerr = 29;
			                  xerrwv("dbolsm(). the offset=(i1) beyond position\nncols=(i2) must be postive for option number 5.",
			                         nerr,level,2,ioff,ncols,0,rdum,rdum);
			                  mode[0] = -nerr;
					          return;
			              } // if (ioff <= 0)

			              fac = x[ncols+ioff];
			              if ( fac < 0.0 ) {
			                  nerr = 30;
			                  nlevel = 0;
			                  xerrwv("dbolsm(). the factor (ncols/mrows) where pre-triangularizing is performed must be nonnegative.\nnow=(r1).",
			                		  nerr,nlevel,0,idum,idum,1,fac,rdum);
			                  mode[0] = -nerr;
					          return;
			              } // if (fac < 0.0)
			          } // if (ip > 0)

			          lds = 2;
			          do580 = true;
			          continue loop;
			      } // else if (jp == 5)
			      else if ( jp == 6) {
			          // 
			          //  CHANGE THE WEIGHTING FACTOR (FROM ONE) TO APPLY TO COMPONENTS
			          //  NUMBERED .GT. MVAL (INITIALLY SET TO 1.)  THIS TRICK IS NEEDED
			          //  FOR APPLICATIONS OF THIS SUBPROGRAM TO THE HEAVILY WEIGHTED
			          //  LEAST SQUARES PROBLEM THAT COME FROM EQUALITY CONSTRAINTS.
			          //
			          if ( ip > 0) {
			              ioff = iopt[lp+2];
			              mval = iopt[lp+3];
			              wt = x[ncols+ioff];
			          } // if (ip > 0)

			          if ( mval < 0 || mval > minput || wt<= 0.0 ) {
			              nerr = 38;
			              nlevel = 0;
			              xerrwv("dbolsm(). the row separator to apply weighting (i1)\nmust lie between 0 and mrows (i2). weight (r1) must be positive.",
			                      nerr,nlevel,2,mval,minput,1,wt,rdum);
			              mode[0] = -nerr;
					      return;
			          } // if ( mval < 0 || mval > minput || wt<= 0.0 )

			          lds = 3;
			          do580 = true;
			          continue loop;
			      } // else if (jp == 6)
			      //
			      //  TURN ON DEBUG OUTPUT.
			      //
			      else if ( jp == 7) {
			          if ( ip > 0) {
			    	      iprint = 1;
			          }
			          lds = 1;
			          do580 = true;
			          continue loop;
			      } // else if (jp == 7)
			      else {
			          nerr = 23;
			          xerrwv("dbolsm. the option number=(i1) is not defined.",
			                  nerr,level,1,ip,idum,0,rdum,rdum);
			          mode[0] = -nerr;
				      return;
			      } // else
			  } // if (do580)
	  } // loop while (true)

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
	
	private void dgeco (double a[][], int lda, int n, int ipvt[], double rcond[], double z[] ) {

	/*****************************************************************************80
	!
	!! DGECO factors a real matrix and estimates its condition number.
	!
	!  Discussion:
	!
	!    If RCOND is not needed, DGEFA is slightly faster.
	!
	!    To solve A * X = B, follow DGECO by DGESL.
	!
	!    To compute inverse ( A ) * C, follow DGECO by DGESL.
	!
	!    To compute determinant ( A ), follow DGECO by DGEDI.
	!
	!    To compute inverse ( A ), follow DGECO by DGEDI.
	!
	!    For the system A * X = B, relative perturbations in A and B
	!    of size EPSILON may cause relative perturbations in X of size
	!    EPSILON/RCOND.
	!
	!    If RCOND is so small that the logical expression
	!      1.0D+00 + RCOND == 1.0D+00
	!    is true, then A may be singular to working precision.  In particular,
	!    RCOND is zero if exact singularity is detected or the estimate
	!    underflows.
	!
	!  Modified:
	!
	!    17 May 2005
	!
	!  Author:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart.
	!
	!    FORTRAN90 translation by John Burkardt.
	!
	!  Reference:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart,
	!    LINPACK User's Guide,
	!    SIAM, (Society for Industrial and Applied Mathematics),
	!    3600 University City Science Center,
	!    Philadelphia, PA, 19104-2688.
	!    ISBN 0-89871-172-X
	!
	!  Parameters:
	!
	!    Input/output, real ( kind = 8 ) A(LDA,N).  On input, a matrix to be
	!    factored.  On output, the LU factorization of the matrix.
	!
	!    Input, integer LDA, the leading dimension of the array A.
	!
	!    Input, integer N, the order of the matrix A.
	!
	!    Output, integer IPVT(N), the pivot indices.
	!
	!    Output, real ( kind = 8 ) RCOND, an estimate of the reciprocal
	!    condition number of A.
	!
	!    Output, real ( kind = 8 ) Z(N), a work vector whose contents are usually
	!    unimportant.  If A is close to a singular matrix, then Z is an
	!    approximate null vector in the sense that
	!      norm ( A * Z ) = RCOND * norm ( A ) * norm ( Z ).
	*/

	  //double a(lda,n)
	  double anorm;
	  double ek;
	  int info[] = new int[1];
	  //integer ipvt(n)
	  int j;
	  int k;
	  int l;
	  double s;
	  double sm;
	  double t;
	  double wk;
	  double wkm;
	  double ynorm;
	  double asum;
	  double zsum;
	  double azsum;
	  //double z(n)
	  //
	  //  Compute the L1 norm of A.
	  //
	  anorm = 0.0;
	  for (j = 1; j <= n; j++) {
		asum = 0.0;
		for (k = 1; k <= n; k++) {
			asum = asum + Math.abs(a[k][j]);
		}
	    anorm = Math.max ( anorm, asum );
	  } // for (j = 1; j <= n; j++)
	  //
	  //  Compute the LU factorization.
	  //
	  dgefa ( a, lda, n, ipvt, info );
	  //
	  //  RCOND = 1 / ( norm(A) * (estimate of norm(inverse(A))) )
	  //
	  //  estimate of norm(inverse(A)) = norm(Z) / norm(Y)
	  //
	  //  where
	  //    A * Z = Y
	  //  and
	  //    A' * Y = E
	  //
	  //  The components of E are chosen to cause maximum local growth in the
	  //  elements of W, where U'*W = E.  The vectors are frequently rescaled
	  //  to avoid overflow.
	  //
	  //  Solve U' * W = E.
	  //
	  ek = 1.0;
	  for (j = 1; j <= n; j++) {
	      z[j] = 0.0;
	  }

	  for (k = 1; k <= n; k++) {

	    if ( z[k] != 0.0 ) {
	    	if (-z[k] > 0) {
	    		ek = Math.abs(ek);
	    	}
	    	else {
	    		ek = -Math.abs(ek);
	    	}
	    } // if ( z[k] != 0.0 )

	    if ( Math.abs ( a[k][k] ) < Math.abs ( ek - z[k] ) ) {
	      s = Math.abs ( a[k][k] ) / Math.abs ( ek - z[k] );
	      for (j = 1; j <= n; j++) {
	          z[j] = s * z[j];
	      }
	      ek = s * ek;
	    } // if ( Math.abs ( a[k][k] ) < Math.abs ( ek - z[k] ) )

	    wk = ek - z[k];
	    wkm = -ek - z[k];
	    s = Math.abs ( wk );
	    sm = Math.abs ( wkm );

	    if ( a[k][k] != 0.0) {
	      wk = wk / a[k][k];
	      wkm = wkm / a[k][k];
	    }
	    else {
	      wk = 1.0;
	      wkm = 1.0;
	    }

	    if ( k+1 <= n ) {

	      for (j = k+1; j <= n; j++) {
	        sm = sm + Math.abs ( z[j] + wkm * a[k][j] );
	        z[j] = z[j] + wk * a[k][j];
	        s = s + Math.abs ( z[j] );
	      } // for (j = k+1; j <= n; j++)

	      if ( s < sm ) {
	        t = wkm - wk;
	        wk = wkm;
	        for (j = k+1; j <= n; j++) {
	            z[j] = z[j] + t * a[k][j];
	        }
	      } // if (s < sm)

	    } // if ( k+1 <= n )

	    z[k] = wk;

	  } // for (k = 1; k <= n; k++)

	  zsum = 0.0;
	  for (j = 1; j <= n; j++) {
		  zsum = zsum + Math.abs(z[j]);
	  }
	  for (j = 1; j <= n; j++) {
	      z[j] = z[j] / zsum;
	  }
	  //
	  //  Solve L' * Y = W
	  //
	  for (k = n; k >=  1; k--) {
        azsum = 0.0;
        for (j = k+1; j <= n; j++) {
        	azsum = azsum + a[j][k] * z[j];
        }
	    z[k] = z[k] + azsum;

	    if ( 1.0 < Math.abs ( z[k] ) ) {
	      for (j = 1; j <= n; j++) {
	          z[j] = z[j] / Math.abs ( z[k] );
	      }
	    } // if ( 1.0 < Math.abs ( z[k] ) )

	    l = ipvt[k];

	    t = z[l];
	    z[l] = z[k];
	    z[k] = t;

	  } // for (k = n; k >=  1; k--)

	  zsum = 0.0;
	  for (j = 1; j <= n; j++) {
		  zsum = zsum + Math.abs(z[j]);
	  }
	  for (j = 1; j <= n; j++) {
	      z[j] = z[j] / zsum;
	  }

	  ynorm = 1.0;
	  //
	  //  Solve L * V = Y.
	  //
	  for (k = 1; k <= n; k++) {

	    l = ipvt[k];

	    t = z[l];
	    z[l] = z[k];
	    z[k] = t;
        
	    for (j = k+1; j <= n; j++) {
	        z[j] = z[j] + t * a[j][k];
	    }

	    if ( 1.0 < Math.abs ( z[k] ) ) {
	      ynorm = ynorm / Math.abs ( z[k] );
	      for (j = 1; j <= n; j++) {
	          z[j] = z[j] / Math.abs ( z[k] );
	      }
	    }

	  } // for (k = 1; k <= n; k++)

	  s = 0.0;
	  for (j = 1; j <= n; j++) {
		  s = s + Math.abs(z[j]);
	  }
	  for (j = 1; j <= n; j++) {
	      z[j] = z[j] / s;
	  }
	  ynorm = ynorm / s;
	  //
	  //  Solve U * Z = V.
	  //
	  for (k = n; k >= 1; k--) {

	    if ( Math.abs ( a[k][k] ) < Math.abs ( z[k] ) ) {
	      s = Math.abs ( a[k][k] ) / Math.abs ( z[k] );
	      for (j = 1; j <= n; j++) {
	          z[j] = s * z[j];
	      }
	      ynorm = s * ynorm;
	    } // if ( Math.abs ( a[k][k] ) < Math.abs ( z[k] ) )

	    if ( a[k][k] != 0.0 ) {
	      z[k] = z[k] / a[k][k];
	    }
	    else {
	      z[k] = 1.0;
	    }

	    for (j = 1; j <= k-1; j++) {
	        z[j] = z[j] - z[k] * a[j][k];
	    }

	  } // for (k = n; k >= 1; k--)
	  //
	  //  Normalize Z in the L1 norm.
	  //
	  zsum = 0.0;
	  for (j = 1; j <= n; j++) {
		  zsum = zsum + Math.abs(z[j]);
	  }
	  s = 1.0 / zsum;
	  for (j = 1; j <= n; j++) {
	      z[j] = s * z[j];
	  }
	  ynorm = s * ynorm;

	  if ( anorm != 0.0) {
	    rcond[0] = ynorm / anorm;
	  }
	  else {
	    rcond[0] = 0.0;
	  }

	  return;
    } // dgeco
	
	private void dgefa (double a[][], int lda, int n, int ipvt[], int info[] ) {

	/*****************************************************************************80
	!
	!! DGEFA factors a real general matrix.
	!
	!  Modified:
	!
	!    07 March 2001
	!
	!  Author:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart.
	!
	!    FORTRAN90 translation by John Burkardt.
	!
	!  Reference:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart,
	!    LINPACK User's Guide,
	!    SIAM, (Society for Industrial and Applied Mathematics),
	!    3600 University City Science Center,
	!    Philadelphia, PA, 19104-2688.
	!    ISBN 0-89871-172-X
	!
	!  Parameters:
	!
	!    Input/output, double A(LDA,N).
	!    On intput, the matrix to be factored.
	!    On output, an upper triangular matrix and the multipliers used to obtain
	!    it.  The factorization can be written A=L*U, where L is a product of
	!    permutation and unit lower triangular matrices, and U is upper triangular.
	!
	!    Input, integer LDA, the leading dimension of A.
	!
	!    Input, integer N, the order of the matrix A.
	!
	!    Output, integer IPVT(N), the pivot indices.
	!
	!    Output, integer INFO, singularity indicator.
	!    0, normal value.
	!    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
	!    but it does indicate that DGESL or DGEDI will divide by zero if called.
	!    Use RCOND in DGECO for a reliable indication of singularity.
	*/


	  //double a(lda,n)
	  int j;
	  int k;
	  int l;
	  double t;
	  double arr[];
	  double arr2[];
	  int m;
	  //
	  //  Gaussian elimination with partial pivoting.
	  //
	  info[0] = 0;

	  for (k = 1; k <= n - 1; k++) {
	      //
	      //  Find L = pivot index.
	      //
		  arr = new double[n-k+2];
		  for (j = 1; j <= n-k+1; j++) {
			  arr[j] = a[k+j-1][k];
		  }
	      l = idamax ( n-k+1, arr, 1 ) + k - 1;
	      ipvt[k] = l;
	      //
	      //  Zero pivot implies this column already triangularized.
	      //
	      if ( a[l][k] == 0.0) {
	          info[0] = k;
	          continue;
	      } // if (a[l][k] == 0.0)
	      //
	      //  Interchange if necessary.
	      //
	      if ( l != k ) {
	          t = a[l][k];
	          a[l][k] = a[k][k];
	          a[k][k] = t;
	      } // if (l != k)
	      //
	      //  Compute multipliers.
	      //
	      t = -1.0 / a[k][k];
	      for (j = 1; j <= n-k; j++) {
	    	  a[k+j][k] = t * a[k+j][k];
	      }
	      //
	      //  Row elimination with column indexing.
	      //
	      for (j = k+1; j <= n; j++) {
	          t = a[l][j];
	          if ( l != k ) {
	              a[l][j] = a[k][j];
	              a[k][j] = t;
	          } // if (l != k)
	          arr = new double[n-k+1];
	          arr2 = new double[n-k+1];
	          for (m = 1; m <= n-k; m++) {
	        	  arr[m] = a[k+m][k];
	        	  arr2[m] = a[k+m][j];
	          }
	          daxpy ( n-k, t, arr, 1, arr2, 1 );
	          for (m = 1; m <= n-k; m++) {
	        	  a[k+m][j] = arr2[m];
	          }
	      } // for (j = k+1; j <= n; j++)

	  } // for (k = 1; k <= n - 1; k++)

	  ipvt[n] = n;

	  if ( a[n][n] == 0.0) {
	    info[0] = n;
	  }

	  return;
    } // dgefa
	
	private void dgesl (double a[][], int lda, int n, int ipvt[], double b[], int job ) {

	/*****************************************************************************80
	!
	!! DGESL solves a real general linear system A * X = B.
	!
	!  Discussion:
	!
	!    DGESL can solve either of the systems A * X = B or A' * X = B.
	!
	!    The system matrix must have been factored by DGECO or DGEFA.
	!
	!    A division by zero will occur if the input factor contains a
	!    zero on the diagonal.  Technically this indicates singularity
	!    but it is often caused by improper arguments or improper
	!    setting of LDA.  It will not occur if the subroutines are
	!    called correctly and if DGECO has set 0.0 < RCOND
	!    or DGEFA has set INFO == 0.
	!
	!  Modified:
	!
	!    07 March 2001
	!
	!  Author:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart.
	!
	!    FORTRAN90 translation by John Burkardt.
	!
	!  Reference:
	!
	!    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart,
	!    LINPACK User's Guide,
	!    SIAM, (Society for Industrial and Applied Mathematics),
	!    3600 University City Science Center,
	!    Philadelphia, PA, 19104-2688.
	!    ISBN 0-89871-172-X
	!
	!  Parameters:
	!
	!    Input, real ( kind = 8 ) A(LDA,N), the output from DGECO or DGEFA.
	!
	!    Input, integer LDA, the leading dimension of A.
	!
	!    Input, integer N, the order of the matrix A.
	!
	!    Input, integer IPVT(N), the pivot vector from DGECO or DGEFA.
	!
	!    Input/output, double B(N).
	!    On input, the right hand side vector.
	!    On output, the solution vector.
	!
	!    Input, integer JOB.
	!    0, solve A * X = B;
	!    nonzero, solve A' * X = B.
	*/

	  //double a(lda,n)
	  //double b(n)
	  //integer ipvt(n)
	  int k;
	  int l;
	  double t;
	  double arr[];
	  double arr2[];
	  int j;
	  //
	  //  Solve A * X = B.
	  //
	  if ( job == 0 ) {

	    for (k = 1; k <= n-1; k++) {

	      l = ipvt[k];
	      t = b[l];

	      if ( l != k ) {
	        b[l] = b[k];
	        b[k] = t;
	      }

	      arr = new double[n-k+1];
	      arr2 = new double[n-k+1];
	      for (j = 1; j <= n-k; j++) {
	    	  arr[j] = a[k+j][k];
	    	  arr2[j] = b[k+j];
	      }
	      daxpy ( n-k, t, arr, 1, arr2, 1 );
	      for (j = 1; j <= n-k; j++) {
	    	  b[k+j] = arr2[j];
	      }

	    } // for (k = 1; k <= n-1; k++) 

	    for (k = n; k >= 1; k--) {
	      b[k] = b[k] / a[k][k];
	      t = -b[k];
	      arr = new double[k];
	      for (j = 1; j <= k-1; j++) {
	    	  arr[j] = a[j][k];
	      }
	      daxpy ( k-1, t, arr, 1, b, 1 );
	    } // for (k = n; k >= 1; k--)
	  } // if (job == 0)
	  else {
	    //
	    // Solve A' * X = B.
	    //
	    for (k = 1; k <= n; k++) {
	      arr = new double[k];
	      for (j = 1; j <= k-1; j++) {
	    	  arr[j] = a[j][k];
	      }
	      t = ddot ( k-1, arr, 1, b, 1 );
	      b[k] = ( b[k] - t ) / a[k][k];
	    } // for (k = 1; k <= n; k++)

	    for (k = n-1; k >= 1; k--) {
          arr = new double[n-k+1];
          arr2 = new double[n-k+1];
          for (j = 1; j <= n-k; j++) {
        	  arr[j] = a[k+j][k];
        	  arr2[j] = b[k+j];
          }
	      b[k] = b[k] + ddot ( n-k, arr, 1, arr2, 1 );
	      l = ipvt[k];

	      if ( l != k ) {
	        t = b[l];
	        b[l] = b[k];
	        b[k] = t;
	      }

	    } // for (k = n-1; k >= 1; k--)

	  } // else

	  return;
	} // dgesl
	
	private void difcen (double fj[][], int func, double fx[], int iopt[], int ldfj, int mcon,
			             int mequa, int nvars, double ropt[], double x[] ) {

	/*****************************************************************************80
	!
	!! DIFCEN estimates a jacobian using central differences.
	!
	!  Modified:
	!
	!    18 February 2002
	!
	!  Author:
	!
	!    John Burkardt
	!
	!  Parameters:
	!
	!    Output, double FJ(LDFJ,NVARS), the estimated jacobian.
	!
	!    Input, external FUNC, the name of the user written
	!    function evaluation routine.  FUNC should have the form:
	!      subroutine func ( fx, iopt, mcon, mequa, nvars, ropt, x )
	!    and should accept X as input, and return in FX the value
	!    of the MEQUA+MCON functions.
	!
	!    Workspace, double FX(MEQUA+MCON).
	!
	!    Throughput, integer IOPT(*), parameters to be passed to FUNC.
	!
	!    Input, integer LDFJ, the leading dimension of FJ, which must
	!    be at least MEQUA+MCON.
	!
	!    Input, integer MCON, the number of constraints.
	!
	!    Input, integer MEQUA, the number of nonlinear functions.
	!
	!    Input, integer NVARS, the number of variables.
	!
	!    Throughput, double ROPT(*), parameters to be passed to FUNC.
	!
	!    Input, double X(NVARS), the point at which the
	!    jacobian should be evaluated.
	*/

	  double dxj;
	  double eps;
	  //double fj(ldfj,nvars)
	  //external func
	  //double fx(mequa+mcon)
	  int j;
	  //double x(nvars)
	  double xsave;
	  int k;
	  //
	  //  Get the square root of the machine precision.
	  //
	  eps = Math.sqrt ( epsilon);
	  //
	  //  Consider each component X(J) of the set of variables.
	  //
	  for (j = 1; j <= nvars; j++) {
	      //
	      //  Set the appropriate increment DXJ to X(J).
	      //
	      dxj = eps * ( Math.abs ( x[j] ) + 1.0 );
	      //
	      //  Make a copy XP of X, with X(J) incremented by DXJ.
	      //
	      xsave = x[j];

	      x[j] = xsave + dxj;
	      //
	      //Evaluate F(XP).
	      //
	      //func ( fx, iopt, mcon, mequa, nvars, ropt, x )
	      //
	      //  Save F(XP).
	      //
	      for (k = 1; k <= mequa+mcon; k++) {
	          fj[k][j] = fx[k];
	      }
	      //
	      // Make a copy XM of X, with X(J) decremented by DXJ.
	      //
	      x[j] = xsave - dxj;
	      //
	      //  Evaluate F(XM).
	      //
	      //func ( fx, iopt, mcon, mequa, nvars, ropt, x )
	      //
	      // Estimate the partial derivative d F/d X(J) by (F(XP)-F(XM))/(2*DXJ)
	      //
	      for (k = 1; k <= mequa+mcon; k++) {
	          fj[k][j] = ( fj[k][j] - fx[k] ) / ( 2.0 * dxj );
	      }
	      //
	      //  Restore the value of X(J).
	      //
	      x[j] = xsave;

	  } // for (j = 1; j <= nvars; j++)

	  return;
   } // difcen
	
	private void diffor (double fj[][], int func, double fx[], int iopt[], int ldfj,
			             int mcon, int mequa, int nvars, double ropt[], double x[] ) {

	/*****************************************************************************80
	!
	!! DIFFOR estimates a jacobian using forward differences.
	!
	!  Modified:
	!
	!    18 February 2002
	!
	!  Author:
	!
	!    John Burkardt
	!
	!  Parameters:
	!
	!    Output, double FJ(LDFJ,NVARS), the estimated jacobian.
	!
	!    Input, external FUNC, the name of the user written
	!    function evaluation routine.  FUNC should have the form:
	!      subroutine func ( fx, iopt, mcon, mequa, nvars, ropt, x )
	!    and should accept X as input, and return in FX the value
	!    of the MEQUA+MCON functions.
	!
	!    Workspace, double FX(MEQUA+MCON).
	!
	!    Throughput, integer IOPT(*), parameters to be passed
	!    to FUNC.
	!
	!    Input, integer LDFJ, the leading dimension of FJ, which must
	!    be at least MEQUA+MCON.
	!
	!    Input, integer MCON, the number of constraints.
	!
	!    Input, integer MEQUA, the number of nonlinear functions.
	!
	!    Input, integer NVARS, the number of variables.
	!
	!    Throughput, double ROPT(*), parameters to be passed to FUNC.
	!
	!    Input, double X(NVARS), the point at which the
	!    jacobian should be evaluated.
	*/

	  double dxj;
	  double eps;
	  //double fj(ldfj,nvars)
	  //external func
	  //double fx(mequa+mcon)
	  int j;
	  //double x(nvars)
	  double xsave;
	  int k;
	  //
	  // Evaluate F(X) and save it in FX.
	  //
	  //func ( fx, iopt, mcon, mequa, nvars, ropt, x )
	  //
	  //  Get the square root of the machine precision.
	  //
	  eps = Math.sqrt ( epsilon );
	  //
	  //  Consider each component X(J) of the set of variables.
	  //
	  for (j = 1; j <= nvars; j++) {
	      //
	      //  Set the appropriate increment DXJ to X(J).
	      //
	      dxj = eps * ( Math.abs ( x[j] ) + 1.0 );
	      //
	      //  Make a copy XP of X, with X(J) incremented by DXJ.
	      //
	      xsave = x[j];

	      x[j] = xsave + dxj;
	      //
	      //  Evaluate F(XP) and store it in column J.
	      //
	      //func ( fj(1,j), iopt, mcon, mequa, nvars, ropt, x )
	      //
	      // Estimate the partial derivative d F/d X(J) by (F(XP)-F(X))/DXJ
	      //
	      for (k = 1; k <= mequa+mcon; k++) {
	          fj[k][j] = ( fj[k][j] - fx[k] ) / dxj;
	      }
	      //
	      //  Restore the value of X(J).
	      //
	      x[j] = xsave;

	  } //  for (j = 1; j <= nvars; j++)

	  return;
   } // diffor
	
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

	  //double a(lda,n)
	  int i;
	  char icol[] = new char[4];
	  int j;
	  int k1;
	  int k2;
	  int ndigit;
	  boolean do10 = false;
	  boolean do20 = false;
	  boolean do40 = false;
	  boolean do60 = false;
	  boolean do80 = false;
	  boolean do100 = false;
	  boolean do120 = false;
	  boolean do140 = false;
	  icol[1] = 'c';
	  icol[2] = 'o';
	  icol[3] = 'l';
	  String iStr;
	  int sLen;
	  int padLen;
	  int p;
	  DecimalFormat dfi = new DecimalFormat("###0");
	  DecimalFormat df1 = new DecimalFormat("###0.00000E000");
	  DecimalFormat df2 = new DecimalFormat("###0.0000000000000E000");
	  DecimalFormat df3 = new DecimalFormat("###0.0000000000000000000E000");
	  DecimalFormat df4 = new DecimalFormat("###0.000000000000000000000000000E000");

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
	      ndigit = -idigit;
	      if ( ndigit > 6) {
	    	  do20 = true;
	      }
	      else {
	    	  do10 = true;
	      }
	  }

	  if (do10) {
	      for (k1=1; k1 <= n; k1 += 4) {
	          k2 = Math.min(n,k1+3);
	          Preferences.debug("          ");
	          for (i = k1; i <= k2; i++) {
	        	  Preferences.debug("     col");
	        	  Preferences.debug(dfi.format(i) + "  ");
	        	  if  ((i == k2) || (((i - k1) % 8) == 7)) {
	        		  Preferences.debug("\n");
	        	  }
	        	  if ((i < k2) && (((i - k1) % 8) == 7)) {
	        		  Preferences.debug("          ");  
	        	  }
	          }
	          //write(*,1000) (icol,i,i=k1,k2)
	          for (i=1; i <= m; i++) {
	              Preferences.debug(" row");
	        	  Preferences.debug(dfi.format(i) + "  ");
	        	  for (j = k1; j <= k2; j++) {
	        		  Preferences.debug(df1.format(a[i][j]));
	        	  }
	        	  Preferences.debug("\n");
	              //write(*,1004) i,(a(i,j),j=k1,k2)
	          } // for (i = 1; i <= m; i++)
	      } // for (k1=1; k1 <= n; k1 += 4)

	  return;
	  } // if (do10)

	  if (do20) {
		  if ( ndigit > 14)  {
			  do40 = true;
		  }
		  else {
			  for (k1=1; k1 <= n; k1 += 2) {
			    k2 = Math.min ( n, k1+1 );
			    Preferences.debug("          ");
			    for (i = k1; i <= k2; i++) {
		        	  Preferences.debug("         col");
		        	  Preferences.debug(dfi.format(i) + "      ");
		        	  if  ((i == k2) || (((i - k1) % 5) == 4)) {
		        		  Preferences.debug("\n");
		        	  }
		        	  if ((i < k2) && (((i - k1) % 5) == 4)) {
		        		  Preferences.debug("          ");  
		        	  }
		          }
			    //write(*,1001) (icol,i,i=k1,k2)
			    for (i = 1; i <= m; i++) {
			    	Preferences.debug(" row");
		        	Preferences.debug(dfi.format(i) + "  ");
		        	for (j = k1; j <= k2; j++) {
		        		Preferences.debug(df2.format(a[i][j]));
		        	}
		        	Preferences.debug("\n");
			      //write(*,1005) i,(a(i,j),j=k1,k2)
			    } // for (i = 1; i <= m; i++)
			  } // for (k1=1; k1 <= n; k1 += 2)
	
		  return;
		  } // else 
	  } // if (do20)

	  if (do40) {
		  if ( ndigit > 20) {
			  do60 = true;
		  }
		  else {
			  for (k1=1; k1 <= n; k1 += 2) {
			    k2 = Math.min(n,k1+1);
			    Preferences.debug("          ");
			    for (i = k1; i <= k2; i++) {
		        	  Preferences.debug("            col");
		        	  Preferences.debug(dfi.format(i) + "         ");
		        	  if  ((i == k2) || (((i - k1) % 4) == 3)) {
		        		  Preferences.debug("\n");
		        	  }
		        	  if ((i < k2) && (((i - k1) % 4) == 3)) {
		        		  Preferences.debug("          ");  
		        	  }
		          }
			    //write(*,1002) (icol,i,i=k1,k2)
			    for (i = 1; i <= m; i++) {
			    	Preferences.debug(" row");
		        	Preferences.debug(dfi.format(i) + "  ");
		        	for (j = k1; j <= k2; j++) {
		        		Preferences.debug(df3.format(a[i][j]));
		        	}
		        	Preferences.debug("\n");
			      //write(*,1006) i,(a(i,j),j=k1,k2)
			    } // for (i = 1; i <= m; i++)
			  } // for (k1=1; k1 <= n; k1 += 2)
		  return;
		  } // else 
	  } // if (do40)

	  if (do60) {
	      for (k1 = 1; k1 <= n; k1++) {
	          k2 = k1;
	          Preferences.debug("          ");
			  for (i = k1; i <= k2; i++) {
		          Preferences.debug("                col");
		          Preferences.debug(dfi.format(i) + "             ");
		          if  ((i == k2) || (((i - k1) % 3) == 2)) {
		              Preferences.debug("\n");
		          }
		          if ((i < k2) && (((i - k1) % 3) == 2)) {
		              Preferences.debug("          ");  
		          }
			  }
	          //write(*,1003) (icol,i,i=k1,k2)
	          for (i = 1; i <= m; i++) {
	        	  Preferences.debug(" row");
	        	  Preferences.debug(dfi.format(i) + "  ");
	        	  for (j = k1; j <= k2; j++) {
	        		  Preferences.debug(df4.format(a[i][j]));
	        	  }
	        	  Preferences.debug("\n");
	              //write(*,1007) i,(a(i,j),j=k1,k2)
	          } // for (i = 1; i <= m; i++)
	      } // for (k1 = 1; k1 <= n; k1++) 
	  return;
	  } // if (do60)

	  if (do80) {
		  if ( ndigit > 6) {
			  do100 = true;
		  }
		  else {
		      for (k1=1; k1 <= n; k1 +=8) {
		          k2 = Math.min(n,k1+7);
		          Preferences.debug("          ");
		          for (i = k1; i <= k2; i++) {
		        	  Preferences.debug("     col");
		        	  Preferences.debug(dfi.format(i) + "  ");
		        	  if  ((i == k2) || (((i - k1) % 8) == 7)) {
		        		  Preferences.debug("\n");
		        	  }
		        	  if ((i < k2) && (((i - k1) % 8) == 7)) {
		        		  Preferences.debug("          ");  
		        	  }
		          }
		          //write(*,1000) (icol,i,i=k1,k2)
		          for (i=1; i <= m; i++) {
		        	  Preferences.debug(" row");
		        	  Preferences.debug(dfi.format(i) + "  ");
		        	  for (j = k1; j <= k2; j++) {
		        		  Preferences.debug(df1.format(a[i][j]));
		        	  }
		        	  Preferences.debug("\n");
		              //write(*,1004) i,(a(i,j),j=k1,k2)
		          } // for (i = 1; i <= m; i++)
		      } // for (k1 = 1; k1 <= n; k1 += 8)
	          return;
		  } // else 
	  } // if (do80)

	  if (do100) {
	      if ( ndigit > 14) {
	    	  do120 = true;
	      }
	      else {
	          for (k1=1; k1 <= n; k1 += 5) {
	              k2 = Math.min(n,k1+4);
	              Preferences.debug("          ");
			      for (i = k1; i <= k2; i++) {
		        	  Preferences.debug("         col");
		        	  Preferences.debug(dfi.format(i) + "      ");
		        	  if  ((i == k2) || (((i - k1) % 5) == 4)) {
		        		  Preferences.debug("\n");
		        	  }
		        	  if ((i < k2) && (((i - k1) % 5) == 4)) {
		        		  Preferences.debug("          ");  
		        	  }
		          }
	              //write(*,1001) (icol,i,i=k1,k2)
	              for (i = 1; i <= m; i++) {
	            	  Preferences.debug(" row");
			          Preferences.debug(dfi.format(i) + "  ");
			          for (j = k1; j <= k2; j++) {
			        	  Preferences.debug(df2.format(a[i][j]));
			          }
			          Preferences.debug("\n");
	                  //write(*,1005) i,(a(i,j),j=k1,k2)
	              } // for (i = 1; i <= m; i++)
	          } // for (k1 = 1; k1 <= n; k1 += 5)

	          return;
	      } // else
	  } // if (do100)

	  if (do120) {
	      if ( ndigit > 20) {
		      do140 = true;
	      }
	      else {
	          for (k1 = 1; k1 <= n; k1 += 4) {
	              k2 = Math.min(n,k1+3);
	              Preferences.debug("          ");
				  for (i = k1; i <= k2; i++) {
			          Preferences.debug("            col");
			          Preferences.debug(dfi.format(i) + "         ");
			          if  ((i == k2) || (((i - k1) % 4) == 3)) {
			              Preferences.debug("\n");
			          }
			          if ((i < k2) && (((i - k1) % 4) == 3)) {
			              Preferences.debug("          ");  
			          }
			      }
	              //write(*,1002) (icol,i,i=k1,k2)
	              for (i = 1; i <= m; i++) {
	            	  Preferences.debug(" row");
			          Preferences.debug(dfi.format(i) + "  ");
			          for (j = k1; j <= k2; j++) {
			        	  Preferences.debug(df3.format(a[i][j]));
			          }
			          Preferences.debug("\n");
	                  //write(*,1006) i,(a(i,j),j=k1,k2)
	              } // for (i = 1; i <= m; i++)
	          } // for (k1 = 1; k1 <= n; k1 += 4)
	          return;
	      } // else 
	  } // if (do120)

	  if (do140) {

		  for (k1=1; k1 <= n; k1 += 3) {
		    k2 = Math.min(n,k1+2);
		    Preferences.debug("          ");
			for (i = k1; i <= k2; i++) {
		        Preferences.debug("                col");
		        Preferences.debug(dfi.format(i) + "             ");
		        if  ((i == k2) || (((i - k1) % 3) == 2)) {
		            Preferences.debug("\n");
		        }
		        if ((i < k2) && (((i - k1) % 3) == 2)) {
		            Preferences.debug("          ");  
		        }
			}
		    //write(*,1003) (icol,i,i=k1,k2)
		    for (i=1; i <= m; i++) {
		    	Preferences.debug(" row");
	        	Preferences.debug(dfi.format(i) + "  ");
	        	for (j = k1; j <= k2; j++) {
	        		Preferences.debug(df4.format(a[i][j]));
	        	}
	        	Preferences.debug("\n");
		        //write(*,1007) i,(a(i,j),j=k1,k2)
		    } // for (i = 1; i <= m; i++)
		  } // for (k1=1; k1 <= n; k1 += 3)

	      return;
	  } // if (do140)
	 //1000 format(10x,8(5x,3a1,i4,2x))
	 //1001 format(10x,5(9x,3a1,i4,6x))
	 //1002 format(10x,4(12x,3a1,i4,9x))
	 //1003 format(10x,3(16x,3a1,i4,13x))
	 //1004 format(1x,'row',i4,2x,1p8d14.5)
	 //1005 format(1x,'row',i4,2x,1p5d22.13)
	 //1006 format(1x,'row',i4,2x,1p4d28.19)
	 //1007 format(1x,'row',i4,2x,1p3d36.27) 
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
	
	private void dpchek (double df[], int dqedev, double fj[][], int iopt[], int ldfj,
			             int nvars, double ropt[], double x[], double y[] ) {

	/*****************************************************************************80
	!
	!! DPCHEK checks the user's jacobian routine.
	!
	!  Modified:
	!
	!    11 September 2002
	!
	!  Parameters:
	!
	!    Workspace, double DF(NVARS).
	!
	!    Input, external DQEDEV, the name of the user written jacobian
	!    and function evaluation routine.
	!
	!    Workspace, double FJ(LDFJ,NVARS+1), space to store
	!    the jacobian and the function, as required by DQEDEV.
	!
	!    Throughput, integer IOPT(*), parameters to be passed to DQEDEV.
	!
	!    Input, integer LDFJ, the leading dimension of FJ, which must
	!    be at least NVARS.
	!
	!    Input, integer NVARS, the number of variables.
	!
	!    Throughput, double ROPT(*), parameters to be passed to DQEDEV.
	!
	!    Input, double X(NVARS), the point at which the
	!    jacobian should be evaluated.
	!
	!    Workspace, double Y(NVARS).
	*/


	  //double df(nvars)
	  //external dqedev
	  double eps;
	  //double fj(ldfj,nvars+1)
	  int igo[] = new int[1];
	  int j;
	  double t;
	  int k;
	  double arr[];
	  //double x(nvars)
	  //double y(nvars)
	  //
	  //  Get the square root of the machine precision.
	  //
	  eps = Math.sqrt ( epsilon );
	  //
	  //  Consider each component X(J) of the set of variables.
	  //
	  Preferences.debug("DPCHEK:\n");
	  Preferences.debug("Compare user jacobian and function for\n");
	  Preferences.debug("consistency, using finite differences.\n\n");

	  dvout ( nvars, x, "Evaluation point X", -4 );

	  for (j = 1; j <= nvars; j++) {
	      //
	      //  Set the appropriate increment T to X(J).
	      //
	      t = eps * ( Math.abs ( x[j] ) + 1.0 );
	      //
	      //  Make a copy YP of X, with Y(J) incremented by T.
	      //
	      for (k = 1; k <= nvars; k++) {
	          y[k] = x[k];
	      }
	      y[j] = x[j] + t;
	      //
	      //  Evaluate F(YP).
	      //
	      igo[0] = 0;
	      dqedev ( y, fj, ldfj, igo, iopt, ropt );
	      //
	      //  Save F(YP).
	      //
	      for (k = 1; k <= nvars; k++) {
	          df[k] = fj[k][nvars+1];
	      }
	      //
	      //  Make a copy YM of X, with Y(J) decremented by T.
	      //
	      y[j] = x[j] - t;
	      //
	      //  Evaluate F(YM).
	      //
	      igo[0] = 0;
	      dqedev ( y, fj, ldfj, igo, iopt, ropt );
	      //
	      //  Estimate the partial derivative d F/d X(J) by (F(YP)-F(YM))/2*T
	      //
	      for (k = 1; k <= nvars; k++) {
	          df[k] = ( df[k] - fj[k][nvars+1] ) / ( 2.0 * t );
	      }
	      //
	      //  Evaluate the user's formula for the partial derivatives.
	      //
	      igo[0] = 1;
	      dqedev ( x, fj, ldfj, igo, iopt, ropt );

	      dvout(nvars,df,"Numerical derivative",-4);

	      Preferences.debug("Variable number " + j + "\n");
          arr = new double[nvars+1];
          for (k = 1; k <= nvars; k++) {
        	  arr[k] = fj[k][j];
          }
	      dvout(nvars,arr,"Analytic partial",-4);

	  } // for (j = 1; j <= nvars; j++)

	  return;
   } // dpchek
	
	private void dqedev (double x[], double fj[][], int ldfj, int igo[], int iopt[],
			             double ropt[] ) {

	/*****************************************************************************80
	!
	!! DQEDEV evaluates functions being treated by DQED.
	!
	!  Discussion:
	!
	!    The user has NVARS variables X(I), and is trying to minimize
	!    the square root of the sum of the squares of MEQUA functions
	!    F(I)(X), subject to MCON constraints which have the form
	!
	!      BL(I) <= G(I)(X) <= BU(I)
	!
	!    where either the left or right bounding inequality may be dropped.
	!
	!  Parameters:
	!
	!    Input, REAL X(*), an array of length NVARS, containing
	!    the values of the independent variables at which the
	!    functions and partial derivatives should be evaluated.
	!
	!    Output, REAL FJ(LDFJ,NVARS+1).
	!
	!    If IGO is nonzero, then partial derivatives must
	!    be placed in the first NVARS columns of FJ, as follows:
	!
	!      Rows I = 1 to MCON, and columns J = 1 to NVARS
	!      should contain the partials of the I-th constraint
	!      function G(I) with respect to the J-th variable.
	!
	!      Rows I=MCON+1 to MCON+MEQUA, and columns J = 1 to NVARS
	!      should contain the partials of the (I-MCON)-th nonlinear
	!      function F(I-MCON) with respect to the J-th variable.
	!
	!    Regardless of the value of IGO, column NVARS+1 must be
	!    assigned the values of the constraints and functions, as follows:
	!
	!      Rows I = 1 to MCON, column J = NVARS+1, should contain
	!      the value of G(I).
	!
	!      Rows I=MCON+1 to MCON+MEQUA, column J = NVARS+1, should
	!      contain the value of F(I-MCON).
	!
	!    Input, integer LDFJ, the leading dimension of FJ, which
	!    must be at least MCON+MEQUA.
	!
	!    Input/output, integer IGO.
	!
	!    On input, IGO tells the user whether the partial derivatives are needed.
	!
	!      0, the partial derivatives are not needed.
	!      nonzero, the partial derivatives are needed.
	!
	!    On output, the user may reset the input value of IGO if one
	!    of two situations is encountered:
	!
	!      99, the functions, constraints, or partial derivatives
	!          could not be evaluated at the given input point X.  Request
	!          that DQED reject that point, and try a different one.
	!
	!      Any other value, abort the run.
	!
	!    Input, integer IOPT(*), the integer option array.
	!
	!    Input, REAL ROPT(*), the double precision option array.
	C***BEGIN PROLOGUE  DQEDEV
C***REFER TO  DQED
C***ROUTINES CALLED  XERROR,CHRCNT
C***END PROLOGUE  DQEDEV
C     REVISED 870204-1100
C     REVISED YYMMDD-HHMM
C     THIS IS THE SUBPROGRAM FOR EVALUATING THE FUNCTIONS
C     AND DERIVATIVES FOR THE NONLINEAR SOLVER, DQED.
C
C     THE USER PROBLEM HAS MCON CONSTRAINT FUNCTIONS,
C     MEQUA LEAST SQUARES EQUATIONS, AND INVOLVES NVARS
C     UNKNOWN VARIABLES.
C
C     WHEN THIS SUBPROGRAM IS ENTERED, THE GENERAL (NEAR)
C     LINEAR CONSTRAINT PARTIAL DERIVATIVES, THE DERIVATIVES
C     FOR THE LEAST SQUARES EQUATIONS, AND THE ASSOCIATED
C     FUNCTION VALUES ARE PLACED INTO THE ARRAY FJ(*,*).
C     ALL PARTIALS AND FUNCTIONS ARE EVALUATED AT THE POINT
C     IN X(*).  THEN THE SUBPROGRAM RETURNS TO THE CALLING
C     PROGRAM UNIT. TYPICALLY ONE COULD DO THE FOLLOWING
C     STEPS:
C
C  IF(IGO.NE.0) THEN
C     STEP 1. PLACE THE PARTIALS OF THE I-TH CONSTRAINT
C             FUNCTION WITH REPECT TO VARIABLE J IN THE
C             ARRAY FJ(I,J), I=1,...,MCON, J=1,...,NVARS.
C  END IF
C     STEP 2. PLACE THE VALUES OF THE I-TH CONSTRAINT
C             EQUATION INTO FJ(I,NVARS+1).
C  IF(IGO.NE.0) THEN
C     STEP 3. PLACE THE PARTIALS OF THE I-TH LEAST SQUARES
C             EQUATION WITH RESPECT TO VARIABLE J IN THE
C             ARRAY FJ(MCON+I,J), I=1,...,MEQUA,
C             J=1,...,NVARS.
C  END IF
C     STEP 4. PLACE THE VALUE OF THE I-TH LEAST SQUARES
C             EQUATION INTO FJ(MCON+I,NVARS+1).
C     STEP 5. RETURN TO THE CALLING PROGRAM UNIT.
C DQEDEV:
C GLOSSARY OF VARIABLES. NOTATION:
C DUMMY-ARG A dummy argument, that is an argument to this prog. unit.
C /S$A$V$E/ SAV Denotes that this variable is local to the routine
C               and is saved between calls to it.
C INTEGER, REAL, DOUBLE PRECISION, LOGICAL, CHARACTER
C               The types of the variables.
C ADJ-ARR An adjustable array, that is an argument to this prog. unit.
C Name      Memory Status  Type     Argument   Uses and comments.
C                                    Status
C ----      -------------  ----     --------   ------------------
C FJ         DUMMY-ARG     REAL      ADJ-ARY
C IGO        DUMMY-ARG     INTEGER
C IOPT       DUMMY-ARG     INTEGER   ADJ-ARY
C LDFJ       DUMMY-ARG     INTEGER
C NERR                     INTEGER
C NMESS                    INTEGER
C ROPT       DUMMY-ARG     REAL      ADJ-ARY
C X          DUMMY-ARG     REAL      ADJ-ARY
C XMESS                    CHAR*128
      DOUBLE PRECISION FJ(LDFJ,*),X(*),ROPT(*)
      INTEGER IGO,IOPT(*)
      CHARACTER XMESS*128
      XMESS =
     . 'DQED. THE EVALUATOR PROGRAM DQEDEV MUST BE WRITTEN BY THE USER.'
      NERR = 09
      IGO = 17
C
C     THE INTENT HERE IS THAT THE EVALUATOR WILL NOT RETURN
C     FROM THE ERROR PROCESSOR CALL.
      CALL CHRCNT(XMESS,NMESS)
      CALL XERROR(XMESS,NMESS,NERR,01)
      RETURN
      END
	*/

	  //double fj(ldfj,*)

	  Preferences.debug("DQEDEV - Fatal error\n");
	  System.out.println("DQEDEV - Fatal error");
	  //write ( *, '(a)' ) '  DQEDEV must be written by the user.'

	  System.exit(-1);
	} // dqedev
	
	private void dqedip (int mequa, int nvars, int mcon, int ind[], double bl[], double bu[], 
			             double x[], double fjac[][], int ldfjac, double fb[], int igo[], int iopt[],
			             double ropt[], int iwa[], double wa[], double dx[], double xb[],
			             double b[], double bb[], double blb[], double bub[], int indb[] ) {

			/*****************************************************************************80
			!
			!! DQEDIP carries out the work of DQEDGN.
			!
			!  Modified:
			!
			!    28 July 2006
			!
			*/

			  double alb;
			  double alfac = 0.0;
			  double alpha = 0.0;
			  double aub;
			  double bboost = 0.0;
			  double bold;
			  double c1516 = 0.0;
			  double chg = 0.0;
			  double chgfac;
			  double colnrm;
			  double dxnrm;
			  double fc = 0.0;
			  //double fjac(ldfjac,*)
			  double fl = 0.0;
			  boolean fulnwt;
			  double gval;
			  int icase;
			  int igotfc;
			  int igotnc;
			  int ipls = 0;
			  int iprint = 0;
			  int iters = 0;
			  int itmax;
			  int j;
			  int jp;
			  int k = 0;
			  int kl = 0;
			  int kp;
			  int level;
			  int lp;
			  int lpdiff;
			  int mode[] = new int[1];
			  int nall = 0;
			  int nerr;
			  boolean newbst;
			  boolean newopt;
			  boolean passb = false;
			  double pb;
			  double pd;
			  double pv[] = new double[1];
			  double rb;
			  double rdum;
			  boolean retrea = false;
			  double rg;
			  double rnormc[] = new double[1];
			  double semibg = 0.0;
			  double t;
			  double t2 = 0.0;
			  boolean term = false;
			  double told;
			  double tolf;
			  double tolp;
			  double tolsnr;
			  double tolx;
			  String xmess;
			  boolean do20 = false;
			  boolean do30 = false;
			  boolean do50 = false;
			  boolean do60 = false;
			  boolean do70 = false;
			  boolean do90 = false;
			  boolean do110 = false;
			  boolean do120 = false;
			  boolean do140 = false;
			  boolean do170 = false;
			  boolean do180 = false;
			  boolean do185 = false;
			  boolean do190 = false;
			  boolean do250 = false;
			  boolean do260 = false;
			  boolean do270 = false;
			  boolean do280 = false;
			  boolean do300 = false;
			  boolean do305 = false;
			  boolean do310 = false;
			  boolean do320 = false;
			  boolean do330 = false;
			  boolean do340 = false;
			  boolean do350 = false;
			  boolean do390 = false;
			  boolean do400 = false;
			  boolean do440 = false;
			  boolean do450 = false;
			  boolean do470 = false;
			  double arr[];
			  int iarr[];
			  int m;
			  DecimalFormat dfi3 = new DecimalFormat("##0");
			  DecimalFormat dfi4 = new DecimalFormat("###0");
			  DecimalFormat df10p4 = new DecimalFormat("0.0000E000");
			  DecimalFormat df12p4 = new DecimalFormat("##0.0000E000");
              DecimalFormat df12p5 = new DecimalFormat("#0.00000E000");
              DecimalFormat df14p4 = new DecimalFormat("####0.0000E000");
			//
			//     OPTIONS:
			//
			//     1    SET THE PRINTED OUTPUT OFF/ON.  REQUIRES TWO ENTRIES
			//          IN IOPT(*).  IF IOPT(*+1)=0, NO PRINTING; =1 PRINT.
			//          (THIS PRINTOUT SHOWS VARIOUS QUANTITIES ASSOCIATED
			//          WITH THE NONLINEAR PROBLEM BEING SOLVED. GOOD FOR
			//          DEBUGGING A PROBLEM IN CASE OF TROUBLE.
			//
			//     2    SET THE MAXIMUM NUMBER OF INTERATIONS.  REQUIRES TWO ENTRIES
			//          IN IOPT(*).  USE IOPT(*+1)= MAXIMUM NUMBER OF ITERATIONS.
			//
			//     3    PASS INITIAL BOUNDS FOR THE TRUST REGION TO THE NONLINEAR
			//          SOLVER. REQUIRES TWO ENTRIES IN IOPT(*). USE IOPT(*+1) AS A
			//          POINTER INTO ROPT(*) FOR START OF THE NVARS BOUNDS.
			//
			  rdum = 0.0;

			  if (iflag_dqedip == 1) {
				  do50 = true;
			  }
			  else { 
			      //
			      //  PROCESS OPTION ARRAY
			      //
			      do470 = true;
			  }
	loop:     while (true) {
			      
                  if (do20) {
			          do20 = false;
			          do30 = true;
			          //
			          //  SET SO X(*)-DX(*) UPDATE WILL BE CORRECT FIRST TIME.
			          //
			          for (j = 1; j <= nvars; j++) {
			              dx[j] = 0.0;
			          }
			          k = 0;
			          //
			          //  FB = "INFINITY" ON THIS MACHINE.
			          //
			          fb[0] = Double.MAX_VALUE;
			          dxnrm = fb[0];
			          fl = 0.0;
			          //
			          //  LINEAR PROBLEM RESIDUAL.
			          //
			          pv[0] = 0.0;
			          retrea = false;
			          fulnwt = false;
			          term = false;
                  } // if (do20)
                  
                  if (do30) {
			          do30 = false;

			          if (! retrea) {
			        	  iters = iters + 1;
			          }
			          if ( retrea) {
			              //
			              //  MUST RETREAT TO BEST X VALUES.
			              //
			              k = 0;
			              kl = -1;
			              fl = fb[0];
			              for (j = 1; j <= nvars; j++) {
			            	  x[j] = xb[j];
			              }
			          }
			          else {
			              kl = k;
			              for (j = 1; j <= nvars; j++) {
			            	  x[j] = x[j] - dx[j];
			              }
			              if ( term) {
			                  iflag_dqedip = 0;
			                  do390 = true;
			              }
			          }
                      if (!do390) {
			              igo[0] = 1;
			              iflag_dqedip = 1;
			              do440 = true;
                      } // if (!do390)
                  } // if (do30)

			     if (do50) {
			    	 do50 = false;
			    	 do400 = true;
			    	 arr = new double[mequa+1];
			    	 for (j = 1; j <= mequa; j++) {
			    		 arr[j] = fjac[mcon+j][nvars+1];
			    	 }
			         fc = dnrm2(mequa,arr,1);
			         //
			         //  TEST FOR CONVERGENCE
			         //
			         igotfc = 60;
			     } // if (do50)

			     if (do60) {

			         if ( term) {
			        	 do60 = false;
			             iflag_dqedip = 0;
			             do390 = true;
			         }
			         else {
			             newbst = fc  <  fb[0] || (mcon > 0 && iters == 2);
			             if ( newbst) {
			            	 k = 0;
			             }
			         }
			     } // if (do60)
			     if ( k == 0 && do60) {
			    	 do60 = false;
			         rg = 0.0;
			         pb = 0.0;
			         pd = Double.MAX_VALUE;
			         //
			         //  WANT TO POSITION AT BEST X VALUES.
			         //
			         fb[0] = fc;
			         
			         switch(2 - kl) {
			         case 1:
			        	 do70 = true;
			        	 break;
			         case 2:
			        	 do90 = true;
			        	 break;
			         case 3:
			        	 do110 = true;
			        	 break;
			         default:
			        	 do120 = true;
			         }

			         if (do70) {
			             do70 = false;
			             do140 = true;
			             //
			             //  IMMEDIATELY GOT A NEW BEST X.
			             //
			             if ( t2 <= 0.25 ) {
			                 bboost = 1.0;
			                 chg = Math.max ( 4.0 * t2, 0.1 );
			             } // if ( t2 <= 0.25 )

			             for (j = 1; j <= nvars; j++) {
			                 bb[j] = chg*bb[j];
			             } // for (j = 1; j <= nvars; j++)
			             //
			             //  THIS CODE FOR ALPHA HAS THE FOLLOWING EFFECT.
			             //  IF FC .EQ. PV, THEN ALPHA=ALFAC.
			             //  IF FC**2 .EQ. PV*FL THEN ALPHA=2.-1./ALFAC
			             //  IF FC**2 IS MUCH LARGER THAN PV*FL, THEN ALPHA=1.
			             //
			             t = fc - pv[0];
			             if ( t == 0.0 ) {
			                 alpha = alfac;
			             }
			             else {
			                 alpha = (pv[0]* (fl-pv[0]))/ (fc+pv[0])/ (alfac-1.0);
			                 alpha = (Math.abs(t)+alfac*alpha)/ (Math.abs(t)+alpha);
			             }

			             alfac = 1.5 * alpha;
			             bboost = Math.min(1.5*alpha*bboost,semibg);
			         } // if (do70)

			         if (do90) {
			             do90 = false;
			             do170 = true;
			             //
			             //  AT THE INITIAL X.
			             //
			             alfac = 256.0;

			             for (j = 1; j <= nvars; j++) {
			                 if ( ! passb) {
			                	 bb[j] = -x[j];
			                 }
			                 if ( bb[j] == 0.0 ) {
			                     arr = new double[mequa+1];
			                     for (m = 1; m <= mequa; m++) {
			                    	 arr[m] = fjac[mcon+m][j];
			                     }
			                     colnrm = dnrm2(mequa,arr,1);
			                     if ( colnrm != 0.0 ) {
			                    	 bb[j] = -fc/colnrm;
			                     }
			                 } // if (bb[j] == 0.0)

			                 if ( bb[j] == 0.0 ) {
			                	 bb[j] = -1.0;
			                 }
			                 xb[j] = x[j];
			                 b[j] = bb[j];
			             } // for (j = 1; j <= nvars; j++)

			             alpha = 1.0;
			             bboost = 0.5;
			             do170 = true;
			         } // if (do90)

			         if (do110) {
			             do110 = false;
			             do140 = true;
			             //
			             //  RETREAT TO BEST X.
			             //
			             if ( alfac != 256.0 ) {
			                 alpha = Math.min ( 1.0 / alfac, 0.25 );
			                 alfac = 1.25;
			             }
			             else {
			                 alpha = 0.25*alpha;
			             }

			             bboost = 0.25;
			         } // if (do110)

			         if (do120) {
			             do120 = false;
			             do140 = true;
			             //
			             //  NOT IMMEDIATELY A BEST X.
			             //
			             rb = 0.0;
			             for (j = 1; j <= nvars; j++) {
			                 rb = Math.max(rb,Math.abs((xb[j]-x[j])/bb[j]));
			             } // for (j = 1; j <= nvars; j++)
			             alpha = rb;
			             alfac = 2.0;
			             bboost = ( 8.0 / 7.0 + rg )/ ( 2.0 / 7.0 + rg );
			         } // if (do120)

			         if (do140) {
			             do140 = false;
			             do170 = true;

			             for (j = 1; j <= nvars; j++) {
			                 dx[j] = xb[j] - x[j];
			                 if ( dx[j] == 0.0 ) {
			                     b[j] = alpha*bb[j];
			                 }
			                 else {
			                     xb[j] = x[j];
			                     if (dx[j] >= 0) {
			                    	 b[j] = Math.abs(alpha*bb[j]) + bboost*dx[j];
			                     }
			                     else {
			                    	 b[j] = -Math.abs(alpha*bb[j]) + bboost*dx[j];
			                     }
			                 }
                             if (b[j] >= 0.0) {
                            	 bb[j] = Math.abs(Math.min(Math.sqrt(Double.MAX_VALUE),Math.abs(b[j])));
                             }
                             else {
                            	 bb[j] = -Math.abs(Math.min(Math.sqrt(Double.MAX_VALUE),Math.abs(b[j])));	 
                             }
			             } // for (j = 1; j <= nvars; j++)
			         } // if (do140)
	          } // if (k == 0 && do60)
			  else if(do60) {
				  do60 = false;
				  do170 = true;
			      //
			      //  COMPUTE A GAUGE FOR RETREATING IF DID NOT GET A NEW BEST.
			      //
			      if ( k == 1) {
			          pb = pv[0];
			          pd = 1.5 * (fb[0]+pb* (pb/fb[0])) - 4.0 * pb;
			      }

			      alpha = ( 0.5 * fc+fl)/ (fc+fl);
			      chg = Math.min(alpha*chg,t2);
			      chg = Math.max ( chg, 0.1 );

			      for (j = 1; j <= nvars; j++) {
			         b[j] = chg*b[j];
			         if ( k == 1) {
			        	 bb[j] = b[j];
			         }
			      } // 

			  } // else if(do60)

			  if (do170) {
			      do170 = false;
			      do400 = true;
			      //
			      //  TEST FOR CONVERGENCE
			      //
			      igotfc = 180;
			  } // if (do170)

			  if (do180) {
                  do180 = false;
			      if ( term) {
			          iflag_dqedip = 0;
			          do390 = true;
			      }
			      else {
			    	  do185 = true;
			      }
			  }

			  if (do185) {
				  do185 = false;
				  do190 = true;
			      k = k + 1;
			      //
			      //  SOLVE LINEAR BOUNDED PROBLEM.
			      //
			      for (j = 1; j <= nvars; j++) {

			          if ( b[j] < 0.0 ) {

			              alb = b[j];
			              if ( dx[j] == 0.0 ) {
			                  //
			                  //  THIS CASE IS REQD. TO AVOID USING BUB(*) AT THE INITIAL PT.
			                  //
			                  aub = -c1516*alb;
			              }
			              else {
			                  aub = Math.min(-c1516*alb,-dx[j]+bub[j]);
			              }
			          } // if (b[j] < 0.0)
			          else {
			              aub = b[j];

			              if ( dx[j] == 0.0) {
			                  alb = -c1516*aub;
			              }
			              else {
			                  alb = Math.max(-c1516*aub,-dx[j]+blb[j]);
			              }

			          } // else
			          //
			          //  RESTRICT THE STEP FURTHER IF USER GIVES BOUNDS.
			          //
			          icase = ind[j];

			          if ( icase == 1 ) {
			              aub = Math.min(aub,x[j]-bl[j]);
			          }
			          else if ( icase == 2 ) {
			              alb = Math.max(alb,x[j]-bu[j]);
			          }
			          else if ( icase == 3 ) {
			              aub = Math.min(aub,x[j]-bl[j]);
			              alb = Math.max(alb,x[j]-bu[j]);
			          }

			          blb[j] = alb;
			          //
			          //  THIS NEXT LINE IS TO GUARANTEE THAT THE LOWER BOUND
			          //  IS .LE. THE UPPER BOUND.
			          //
			          aub = Math.max(aub,alb);
			          bub[j] = aub;
			          indb[j] = 3;

			      } // for (j = 1; j <= nvars; j++)
			      //
			      //  SEE IF USER HAS GIVEN GENERAL CONSTRAINTS.
			      //
			  } // if (do185)
			  if (do190) {
				  do190 = false;
				  do300 = true;
			      for (j = nvars + 1; j <= nall; j++) {

			          icase = ind[j];
			          gval = fjac[j-nvars][nvars+1];

			          switch(icase) {
			          case 1:
			        	  do250 = true;
			        	  break;
			          case 2:
			        	  do260 = true;
			        	  break;
			          case 3:
			        	  do270 = true;
			        	  break;
			          case 4:
			        	  do280 = true;
			        	  break;
			          default:
			        	  do250 = true;
			          } // switch(icase)

			          if (do250) {
                          do250 = false;
			              blb[j] = - (gval-bl[j]);
			              indb[j] = 1;
			              continue;
			          } // if (do250)

			          if (do260) {
                          do260 = false;
			              bub[j] = - (gval-bu[j]);
			              indb[j] = 2;
			              continue;
			          } // if (do260)

			          if (do270) {
                          do270 = false;
			              blb[j] = - (gval-bl[j]);
			              bub[j] = - (gval-bu[j]);
			              indb[j] = 3;
			              continue;
			          } // if (do270)

			          if (do280) {
			        	  do280 = false;  
			              indb[j] = 4;
			          } // if (do280)
			      } // for (j = nvars + 1; j <= nall; j++)
			  } // if (do190)
			 
		      if (do300) {
		    	  do300 = false;
			      //  SOLVE THE LEAST SQUARES PROBLEM WITH BOUNDS AND LINEAR
			      //  CONSTRAINTS.  THESE BOUNDS CAN COME FROM THE USER OR
			      //  THE ALGORITHM.
			      //
		    	 iarr = new int[iopt.length-ipls + 1];
		    	 for (m = ipls; m < iopt.length; m++) {
		    	     iarr[m-ipls+1] = iopt[m];	 
		    	 }
			     dbocls(fjac,ldfjac,mcon,mequa,nvars,blb,bub,indb,iarr,
			            dx,rnormc,pv,mode,wa,iwa);
			     for (m = ipls; m < iopt.length; m++) {
		    	     iopt[m] = iarr[m-ipls+1];	 
		    	 }

				  if ( iprint > 0) {
					  Preferences.debug("iters= " + dfi3.format(iters) + " fc = " + df10p4.format(fc) + " pv[0] = " + df10p4.format(pv[0]) + 
							     " k = " + dfi4.format(k) + " kl = " + dfi4.format(kl) + " fb = " + df10p4.format(fb) + "\n");
					  Preferences.debug("            alpha = " + df14p4.format(alpha) + " bboost = " + df14p4.format(bboost) + "\n");
				      Preferences.debug(" x = " + df12p4.format(x[1]));
				      for (j = 2; j <= nvars; j++) {
				    	  Preferences.debug("    " + df12p4.format(x[j]));
				      }
				      Preferences.debug("\n");
				      Preferences.debug(" xb = " + df12p4.format(xb[1]));
				      for (j = 2; j <= nvars; j++) {
				    	  Preferences.debug("    " + df12p4.format(xb[j]));
				      }
				      Preferences.debug("\n");
				      Preferences.debug(" dx = " + df12p4.format(dx[1]));
				      for (j = 2; j <= nall; j++) {
				    	  Preferences.debug("    " + df12p4.format(dx[j]));
				      }
				      Preferences.debug("\n");
				      Preferences.debug(" b = " + df12p4.format(b[1]));
				      for (j = 2; j <= nvars; j++) {
				    	  Preferences.debug("    " + df12p4.format(b[j]));
				      }
				      Preferences.debug("\n");
				      Preferences.debug(" blb = " + df12p4.format(blb[1]));
				      for (j = 2; j <= nall; j++) {
				    	  Preferences.debug("    " + df12p4.format(blb[j]));
				      }
				      Preferences.debug("\n");
				      Preferences.debug(" bub = " + df12p4.format(bub[1]));
				      for (j = 2; j <= nall; j++) {
				    	  Preferences.debug("    " + df12p4.format(bub[j]));
				      }
				      Preferences.debug("\n");
				      Preferences.debug("end of iteration\n");
				  } // if (iprnt > 0)
			      //
			      //  TEST FOR NOISE IN LINEAR PROBLEM SOLN.
			      //
			      // NOTE THAT THIS CODE MAKES NO SENSE.  term is always false.
			      term = ( mcon == 0 && (pv[0]>=fc) );
			      term=false;
			      if ( term) {
			          if ( iprint > 0) {
			              Preferences.debug("linear residual>=current f. quitting. pv[0] = " + df12p5.format(pv[0]) +
			              		            "fc = " + df12p5.format(fc) + "\n");
			          }
                      for (j = 1; j <= nvars; j++) {
                    	  x[j] = xb[j];
                      }
			          igo[0] = 4;
			          iflag_dqedip = 0;
			          do390 = true;
		          } // if (term)
			      else {
			    	  do305 = true;
			      }
		      } // if (do300)

			  /*if (do305) {
				  do305 = false;
		          rg = Math.max(rg, (pv[0]-pb)/pd);
			      if ( ! retrea) {
			          chg = 1.0;
			          t2 = 0.0;

			          for (j = 1; j <= nvars; j++) {

			              bold = b[j];
			              t = dx[j]/bold;
			              //
			              // IF USER GIVES BOUNDS, AND THESE BOUNDS ARE HIT,
			              // DO NOT DETRACT FROM DECLARING A FULL NEWTON STEP.
			              //
			              icase = ind[j];
			              switch (icase) {
			              case 1:
			            	  do310 = true;
			            	  break;
			              case 2:
			            	  do320 = true;
			            	  break;
			              case 3:
			            	  do330 = true;
			            	  break;
			              case 4:
			            	  do340 = true;
			            	  break;
			              default:
			            	  do310 = true;
			              } // switch(icase)

			              if (do310) {
			            	  do310 = false;
			                  alb = (x[j]-bl[j])/bold;
			                  aub = -semibg;
			                  do350 = true;
			              } // if (do310)

			              if (do320) {
			            	  do320 = false;
			                  aub = (x[j]-bu[j])/bold;
			                  alb = -semibg;
			                  do350 = true;
			              } // if (do320)

			              if (do330) {
			            	  do330 = false;
			                  alb = (x[j]-bl[j])/bold;
			         aub = (x(j)-bu(j))/bold
			         go to 350
			              } // if (do33)

			  340        continue
			         alb = -semibg
			         aub = -semibg

			  350        continue

			         if ( t == 1.0D+00 ) then
			             t2 = 1.0D+00
			             b(j) = bold + bold
			             chg = chg*chgfac
			         else
			             if ( abs(t) < 0.25D+00 .and. dx(j)/= 0.0D+00 ) then
			                 b(j) = sign(0.25D+00 * bold,dx(j)) + 3.0D+00 * dx(j)
			             else
			                 b(j) = sign(bold,dx(j))
			             end if
			         end if
			!
			!  THIS TEST AVOIDS THE USER BOUNDS IN DECLARING A NEWTON STEP.
			!
			         if ( abs(alb-t)>=0.01D+00*abs(t) .and. &
			              abs(aub-t) >= 0.01D+00*abs(t)) then
			             if ( t > 0.0D+00 ) then
			                 t2 = max(t2,t)
			             else
			                 t2 = max(t2,-t/c1516)
			             end if
			         end if

			          } // for (j = 1; j <= nvars; j++)

			      fulnwt = t2  <  0.99D+00
			      fl = fc
			      dxnrm = abs(dx(idamax(nvars,dx,1)))
			!
			!  TEST FOR SMALL ABSOLUTE CHANGE IN X VALUES.
			!
			      term = dxnrm  <  told .and. fulnwt
			      if ( term) then
			          igo = 5

			          go to 370

			      end if

			      term = dxnrm  <  dnrm2(nvars,x,1)*tolx .and. fulnwt
			      term = term .and. (iters > 1)
			      if ( term) then
			          igo = 6

			          go to 370

			      end if

			      go to 380

			  370     continue
			      go to 30

			      } // if (!retrea)
			  } // if (do305)

			  380 continue
			  go to 30

			  390 continue
			  go to 440
			!
			!  TEST FOR CONVERGENCE
			!
			  400 continue
			!
			!  TEST FOR SMALL FUNCTION NORM.
			!
			  term = fc <= tolf .or. term
			!
			!  IF HAVE CONSTRAINTS MUST ALLOW AT LEAST ONE MOVE.
			!
			  term = term .and. (mcon == 0 .or. iters > 1)
			  if ( term) then
			      igo = 2
			      go to 420
			  end if
			!
			!  TEST FOR NO CHANGE
			!
			  assign 410 to igotnc
			  go to 430

			  410 continue
			  term = term .and. .not. retrea
			  if ( term) then
			      igo = 3
			      go to 420
			  end if

			  term = iters >= itmax
			  if ( term) then
			      igo = 7
			  end if

			  420 continue
			  if (igotfc == 60) {
				  do60 = true;
			  }
			  else if (igotfc = 180) {
				  do180 = true;
			  }
			  continue loop;
			!
			!  TEST FOR NO CHANGE
			!
			  430 continue
			  t = sqrt(max( 0.0D+00, (fl-pv)* (fl+pv)))
			  term = (abs(fb-fc)<=tolsnr*fb) .and. (t.le.pv*tolp)
			  term = term .and. (abs(fc-fl)<=fb*tolsnr)
			  term = term .and. fulnwt

			  go to igotnc

			  440 continue
			  return
			!
			!  INITIALIZE OTHER VALUES
			!
			  450 continue

			  iters = 0
			  nall = mcon + nvars
			  chgfac = 2.0D+00** (-one/ real ( nvars, kind = 8 ))
			  c1516 = 15.0D+00 / 16.0D+00
			  semibg = 1.0D+10
			!
			!  MAKE SURE THAT VARIABLES SATISFY THE BOUNDS AND CONSTRAINTS.
			!
			  do j = 1,nall
			     blb(j) = bl(j)
			     bub(j) = bu(j)
			     indb(j) = ind(j)
			  end do

			  do20 = true;
			!
			!  PROCESS OPTION ARRAY
			!
			  470 continue
			  iprint = 0
			!
			!  T = MACHINE REL. PREC.
			!
			  t = epsilon ( t )
			  tolf = t
			  tolx = tolf
			  told = tolf
			  tolsnr = 1.0D-03
			  tolp = 1.0D-03
			  itmax = 18
			  passb = .false.
			  level = 1
			  ipls = 0
			  lpdiff = 0
			  lp = 1

			  480 continue

			  lp = lp + lpdiff
			  lpdiff = 2
			  kp = iopt(lp)
			  newopt = kp  >  0
			  jp = abs(kp)
			!
			!  SEE IF THIS IS THE LAST OPTION.
			!
			  if ( jp == 99) then
			      if ( newopt) then
			!
			!  THE POINTER TO THE START OF OPTIONS FOR THE LINEAR
			!  SOLVER MUST SATISFY THE REQUIREMENTS FOR THAT OPTION ARRAY.
			!
			          if ( ipls == 0) ipls = lp
			          go to 490
			      else
			          lpdiff = 1
			          go to 480
			      end if
			  end if
			!
			!  CHANGE PRINT OPTION.
			!
			  if ( jp == 1) then
			      if ( newopt) iprint = iopt(lp+1)
			      go to 480
			  end if
			!
			!  SEE IF MAX. NUMBER OF ITERATIONS CHANGING.
			!
			  if ( jp == 2) then
			      if ( newopt) itmax = iopt(lp+1)
			      go to 480
			  end if
			!
			!  SEE IF BOUNDS FOR THE TRUST REGION ARE BEING PASSED.
			!
			  if ( jp == 3) then
			      if ( newopt) then
			          call dcopy(nvars,ropt(iopt(lp+1)),1,bb,1)
			          passb = .true.
			      end if

			      go to 480

			  end if
			!
			!  CHANGE TOLERANCE ON THE LENGTH OF THE RESIDUALS.
			!
			  if ( jp == 4) then
			      if ( newopt) tolf = ropt(iopt(lp+1))
			      go to 480
			  end if
			!
			!  CHANGE TOLERANCE ON THE NORM OF THE RELATIVE
			!  CHANGE TO THE PARAMETERS.
			!
			  if ( jp == 5) then
			      if ( newopt) tolx = ropt(iopt(lp+1))
			      go to 480
			  end if
			!
			!  CHANGE TOLERANCE ON ABSOLUTE CHANGE TO THE PARAMETERS.
			!
			  if ( jp == 6) then
			      if ( newopt) told = ropt(iopt(lp+1))
			      go to 480
			  end if

			  if ( jp == 7) then
			!
			!  CHANGE TOLERANCE FOR RELATIVE AGREEMENT BETWEEN
			!  BEST FUNCTION NORM, LAST FUNCTION NORM AND THE
			!  CURRENT FUNCTION NORM.
			!
			      if ( newopt) tolsnr = ropt(iopt(lp+1))
			      go to 480
			  end if

			  if ( jp == 8) then
			!
			!  CHANGE TOLERANCE FOR AGREEMENT BETWEEN PREDICTED
			!  VALUE OF RESIDUAL NORM AND THE PREVIOUS VALUE OF
			!  THE RESIDUAL NORM.
			!
			      if ( newopt) tolp = ropt(iopt(lp+1))
			      go to 480
			  end if
			!
			!  CHANGE THE PRINT LEVEL IN THE ERROR PROCESSOR.
			!
			  if ( jp == 9) then
			      if ( newopt) level = iopt(lp+1)
			      go to 480

			  end if
			!
			!  PASS AN OPTION ARRAY TO THE CONSTRAINED LINEAR SOLVER.
			!  THIS OPTION IS A POINTER TO THE START OF THE OPTION
			!  ARRAY FOR THE SUBPROGRAM.
			!
			  if ( jp == 10) then
			      if ( newopt) ipls = iopt(lp+1)
			      go to 480
			  end if
			!
			!  MOVE THE PROCESSING POINTER BY THE VALUE IN THE
			!  NEXT ENTRY OF THE OPTION ARRAY.  THIS DEVICE IS
			!  INCLUDED SO THAT PASSING OPTIONS TO LOWER LEVEL
			!  SUBROUTINES IS EASY TO DO.
			!
			  if ( jp == 11) then
			      if ( newopt) lpdiff = iopt(lp+1)
			      go to 480
			  end if
			!
			!  SAW AN OPTION (OR GARBAGE) THAT IS NOT ON THE LIST.
			!
			  xmess = 'dqedip. invalid option processed. i1=iopt(*) entry. i2=iopt(i1).'
			  nerr = 08
			  igo = 16
			  call xerrwv(xmess,nerr,level,2,lp,iopt(lp),0,rdum,rdum)
			  iflag = 0

			  go to 440

			  490 continue
			  do450 = true;*/
	} // loop: while(true)

			 /*9001 format (a4,1p10d12.4/ (4x,10d12.4))
			 9011 format ('0+iter.=',i3,' fc=',1pd10.4,' pv=',1pd10.4,' k=',i4, &
			   ' kl=',i4,' fb=',1pd10.4/12x,'al=',1pd14.4,' bb=',1pd14.4)
			 9021 format (' linear residual>=current f. quitting.',1p2d12.5)*/
    } // dqedip
	
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
	
	private void dswap (int n, double x[], int incx, double y[], int incy ) {

	/*****************************************************************************80
	!
	!! DSWAP interchanges two vectors.
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
	!    Input/output, double X(*), one of the vectors to swap.
	!
	!    Input, integer INCX, the increment between successive entries of X.
	!
	!    Input/output, double Y(*), one of the vectors to swap.
	!
	!    Input, integer INCY, the increment between successive elements of Y.
	*/

	  int i;
	  int ix;
	  int iy;
	  int m;
	  double stemp;

	  if ( n <= 0 ) {
		  
	  }
	  else if ( (incx == 1) && (incy == 1) ) {

	    m = ( n % 3 );

	    for (i = 1; i <= m; i++) {
	      stemp = x[i];
	      x[i] = y[i];
	      y[i] = stemp;
	    } // for (i = 1; i <= m; i++)

	    for (i = m+1; i <= n; i += 3) {

	      stemp = x[i];
	      x[i] = y[i];
	      y[i] = stemp;

	      stemp = x[i + 1];
	      x[i + 1] = y[i + 1];
	      y[i + 1] = stemp;

	      stemp = x[i + 2];
	      x[i + 2] = y[i + 2];
	      y[i + 2] = stemp;

	    } // for (i = m+1; i <= n; i += 3)
	  } // else if ( (incx == 1) && (incy == 1) )
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
	      stemp = x[ix];
	      x[ix] = y[iy];
	      y[iy] = stemp;
	      ix = ix + incx;
	      iy = iy + incy;
	    } // for (i = 1; i <= n; i++)

	  } // else

	  return;
	} // dswap
	
	private void dvout (int n, double dx[], String ifmt, int idigit ) {

	/*****************************************************************************80
	!
	!! DVOUT prints double precision vectors.
	!
	!  Discussion:
	!
	!    This routine tries to print out, neatly, a double precision array.
	!    The heading in the format statement IFMT is printed first.
	!
	!  Example:
	!
	!    PRINT AN ARRAY CALLED (COSTS OF PURCHASES) OF LENGTH 100 SHOWING
	!    6 DECIMAL DIGITS PER NUMBER. THE USER IS RUNNING ON A TIME-SHARING
	!    SYSTEM WITH A 72 COLUMN OUTPUT DEVICE.
	!
	!      double precision COSTS(100)
	!      N = 100
	!      IDIGIT = -6
	!      CALL DVOUT(N,COSTS,'(''1COSTS OF PURCHASES'')',IDIGIT)
	!
	!  Author:
	!
	!    John Wisniewski, Richard Hanson,
	!    Sandia National Laboratory.
	!
	!  Parameters:
	!
	!    Input, integer N, the dimension of the vector.
	!
	!    Input, double DX(N), the vector to be printed.
	!
	!    Input, String IFMT, a heading or title.
	!
	!  IDIGIT  PRINT AT LEAST IABS(IDIGIT) DECIMAL DIGITS PER NUMBER.
	!          THE SUBPROGRAM WILL CHOOSE THAT integer 6,14,20 OR 28
	!          WHICH WILL PRINT AT LEAST IABS(IDIGIT) NUMBER OF
	!          PLACES.  IF IDIGIT.LT.0, 72 PRINTING COLUMNS ARE UTILIZED
	!          TO WRITE EACH LINE OF OUTPUT OF THE ARRAY DX(*). (THIS
	!          CAN BE USED ON MOST TIME-SHARING TERMINALS). IF
	!          IDIGIT.GE.0, 133 PRINTING COLUMNS ARE UTILIZED. (THIS CAN
	!          BE USED ON MOST LINE PRINTERS).
	*/

	  int i;
	  int k1;
	  int k2;
	  int ndigit;
	  DecimalFormat dfi = new DecimalFormat("###0");
	  DecimalFormat df1 = new DecimalFormat("###0.00000E000");
	  DecimalFormat df2 = new DecimalFormat("###0.0000000000000E000");
	  DecimalFormat df3 = new DecimalFormat("###0.0000000000000000000E000");
	  DecimalFormat df4 = new DecimalFormat("###0.000000000000000000000000000E000");

	  Preferences.debug(ifmt + "\n");

	  if ( n <= 0 ) {
	    return;
	  }

	  ndigit = idigit;

	  if ( idigit == 0) {
		  ndigit = 6;
	  }

	  if ( idigit < 0 ) {

	    ndigit = -idigit;

	    if ( ndigit <= 6 ) {

	      for (k1=1; k1 <= n; k1 += 4) {
	        k2 = Math.min(n,k1+3);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df1.format(dx[i]));	
	        }
	        //write(*,1000) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1 += 4)
	    } // if (ndigit <= 6)
	    else if ( ndigit <= 14 ) {

	      for (k1=1; k1 <= n; k1 += 2) {
	        k2 = Math.min(n,k1+1);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df2.format(dx[i]));	
	        }
	        //write(*,1001) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1 = 1; k1 <= n; k1 += 2)
	    } // else if (ndigit <= 14)
	    else if ( ndigit <= 20 ) {

	      for (k1=1; k1 <= n; k1 += 2) {
	        k2=Math.min(n,k1+1);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df3.format(dx[i]));	
	        }
	        //write(*,1002) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1 += 2) 
	    } // else if (ndigit <= 20)
	    else {

	      for (k1=1; k1 <= n; k1++) {
	        k2 = k1;
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df4.format(dx[i]));	
	        }
	        //write(*,1003) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1++)

	    } // else 
	  } // if (idigit < 0)
	  else {

	    if ( ndigit <= 6 ) {

	      for (k1=1; k1 <= n; k1 += 8) {
	        k2 = Math.min(n,k1+7);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df1.format(dx[i]));	
	        }
	        //write(*,1000) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1 += 8)
	    } // if (ndigit <= 6)
	    else if ( ndigit <= 14 ) {

	      for (k1=1; k1 <= n; k1 += 5) {
	        k2 = Math.min(n,k1+4);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df2.format(dx[i]));	
	        }
	        //write(*,1001) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1 += 5)
	    } // else if (ndigit <= 14)
	    else if ( ndigit <= 20 ) {

	      for (k1=1; k1 <= n; k1 += 4) {
	        k2 = Math.min(n,k1+3);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df3.format(dx[i]));	
	        }
	        //write(*,1002) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1 += 4)
	    } // else if ( ndigit <= 20 )
	    else {

	      for (k1=1; k1 <= n; k1 += 3) {
	        k2 = Math.min(n,k1+2);
	        Preferences.debug(dfi.format(k1) + " - " + dfi.format(k2) + " ");
	        for (i = k1; i <= k2; i++) {
	            Preferences.debug(df4.format(dx[i]));	
	        }
	        //write(*,1003) k1,k2,(dx(i),i=k1,k2)
	      } // for (k1=1; k1 <= n; k1 += 3)

	    }

	  } // else 

	  return;
	 //1000 format(1x,i4,' - ',i4,1x,1p8e14.5)
	 //1001 format(1x,i4,' - ',i4,1x,1p5e22.13)
	 //1002 format(1x,i4,' - ',i4,1x,1p4e28.19)
	 //1003 format(1x,i4,' - ',i4,1x,1p3e36.27)
	} // dvout
	
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
	
	private void ivout (int n, int ix[], String title, int idigit ) {

	/*****************************************************************************80
	!
	!! IVOUT prints integer vectors.
	!
	!  Modified:
	!
	!    28 July 2006
	!
	!  Author:
	!
	!    John Wisniewski, Richard Hanson,
	!    Sandia National Laboratory.
	!
	!  Parameters:
	!
	!    Input, integer N, the size of the vector IX.
	!
	!    Input, integer IX(N), the array to be printed.
	!
	!    Input, character ( len = * ) TITLE, a title to be printed.
	!
	!    Input, integer IDIGIT, indicates the number of digits to print.
	!    print up to iabs(idigit) decimal digits per number.
	!    the subprogram will choose that integer 4,6,10 or 14
	!    which will print at least iabs(idigit) number of
	!    places.  if idigit.lt.0, 72 printing columns are utilized
	!    to write each line of output of the array ix(*). (this
	!    can be used on most time-sharing terminals). if
	!    idigit.ge.0, 133 printing columns are utilized. (this can
	!    be used on most line printers).
	*/

	  int k1;
	  int k2;
	  int ndigit;
	  DecimalFormat df4 = new DecimalFormat("###0");
	  DecimalFormat df5 = new DecimalFormat("####0");
	  DecimalFormat df7 = new DecimalFormat("######0");
	  DecimalFormat df11 = new DecimalFormat("##########0");
	  DecimalFormat df15 = new DecimalFormat("##############0");
	  int i;

	  Preferences.debug(title.trim() + "\n");

	  if ( n <= 0 ) {
	    return;
	  }

	  ndigit = idigit;

	  if ( idigit == 0 ) {
	    ndigit = 4;
	  }

	  if ( idigit < 0 ) {

	    ndigit = -idigit;

	    if ( ndigit <= 4 ){
	 
	      for (k1 = 1; k1 <= n; k1 += 10) {
	        k2 = Math.min ( n, k1+9 );
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df5.format(ix[i]));
	        }
	        //write(*,1000) k1, k2, ix(k1:k2)
	      } // for (k1 = 1; k1 <= n; k1 += 10)
	    } // if (ndigit <= 4)
	    else if ( ndigit <= 6) {

	      for (k1=1; k1 <= n; k1 += 7) {
	        k2 = Math.min(n,k1+6);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df7.format(ix[i]));
	        }
	        //write(*,1001) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 7)
	    } // else if (ndigit <= 6)
	    else if ( ndigit <= 10) {

	      for (k1=1; k1 <= n; k1 += 5) {
	        k2=Math.min(n,k1+4);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df11.format(ix[i]));
	        }
	        //write(*,1002) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 5)
	    } // else if (ndigit <= 10)
	    else {

	      for (k1=1; k1 <= n; k1 += 3) {
	        k2 = Math.min(n,k1+2);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df15.format(ix[i]));
	        }
	        //write(*,1003) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 3)

	    } // else
	  } // if (idigit < 0)
	  else {

	    if ( ndigit <= 4 ) {
	 
	      for (k1=1; k1 <= n; k1 += 20) {
	        k2 = Math.min(n,k1+19);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df5.format(ix[i]));
	        }
	        //write(*,1000) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 20)
	    } // else if (ndigit <= 4)
	    else if ( ndigit <= 6) {

	      for (k1=1; k1 <= n; k1 += 15) {
	        k2 = Math.min(n,k1+14);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df7.format(ix[i]));
	        }
	        //write(*,1001) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 15)
	    } // else if ( ndigit <= 6)
	    else if ( ndigit <= 10) {

	      for (k1=1; k1 <= n; k1 += 10) {
	        k2 = Math.min(n,k1+9);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df11.format(ix[i]));
	        }
	        //write(*,1002) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 10)
	    } // else if (ndigit <= 10)
	    else {

	      for (k1=1; k1 <= n; k1 += 7) {
	        k2 = Math.min(n,k1+6);
	        Preferences.debug(" " + df4.format(k1) + " - " + df4.format(k2));
	        for (i = k1; i <= k2; i++) {
	        	Preferences.debug(" " + df15.format(ix[i]));
	        }
	        //write(*,1003) k1,k2, ix(k1:k2)
	      } // for (k1=1; k1 <= n; k1 += 7)

	    } // else

	  } // else

	  return;

	 //1000 format(1x,i4,' - ',i4,20(1x,i5))
	 //1001 format(1x,i4,' - ',i4,15(1x,i7))
	 //1002 format(1x,i4,' - ',i4,10(1x,i11))
	 //1003 format(1x,i4,' - ',i4,7(1x,i15))
	} // ivout
    
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
    !    Input, double R1, R2, the first and second real values.
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