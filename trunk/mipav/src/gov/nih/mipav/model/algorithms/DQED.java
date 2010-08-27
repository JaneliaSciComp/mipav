package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/** This is a port of the FORTRAN90 source code dqed.f90.  DQED solves (square) systems of nonlinear equations,
    or minimizes the residual in a set of nonlinear equations, using least squares.  The user may include 
    simple bounds or linear constraints on variables.  DQED was written by Richard Hanson and Fred Krogh
    of Sandia National Laboratory. */

public abstract class DQED {
	
	
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
	
	/*private void dbocls ( w, mdw, mcon, mrows, ncols, bl, bu, ind, iopt, x, &
			  rnormc, rnorm, mode, rw, iw )

			!*****************************************************************************80
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
			!     contraints:
			!
			!       IOPT(1)=-8 Option is recognized but not changed
			!       IOPT(2)=99
			!       CALL DBOCLS( )
			!
			  implicit none

			  integer mdw

			  logical :: accum
			  real ( kind = 8 ) anorm
			  real ( kind = 8 ) bl(*)
			  real ( kind = 8 ) bu(*)
			  logical checkl
			  real ( kind = 8 ) cnorm
			  real ( kind = 8 ) dasum
			  real ( kind = 8 ) ddot
			  real ( kind = 8 ) dnrm2
			  real ( kind = 8 ) drelpr
			  logical filter
			  integer i
			  integer icase
			  integer idope
			  integer idum
			  integer :: igo = 0
			  integer iiw
			  integer ind(*)
			  integer inrows
			  integer iopt(*)
			  integer ip
			  integer irw
			  integer iscale
			  integer iw(*)
			  integer j
			  integer jopt(5)
			  integer jp
			  integer lds
			  integer lenx
			  integer level
			  integer liopt
			  integer liw
			  integer llb
			  integer lliw
			  integer llrw
			  integer llx
			  integer lmdw
			  integer lndw
			  integer locacc
			  integer locdim
			  integer lopt
			  integer lp
			  integer lrw
			  integer mcon
			  integer mdwl
			  integer mnew
			  integer mode
			  integer modec
			  integer mout
			  integer mrows
			  integer ncols
			  integer nerr
			  real ( kind = 8 ), parameter :: one = 1.0D+00
			  logical pretri
			  real ( kind = 8 ) rdum
			  real ( kind = 8 ) rnorm
			  real ( kind = 8 ) rnormc
			  real ( kind = 8 ) rw(*)
			  real ( kind = 8 ) t
			  real ( kind = 8 ) t1
			  real ( kind = 8 ) t2
			  real ( kind = 8 ) w(mdw,*)
			  real ( kind = 8 ) wt
			  real ( kind = 8 ) x(*)

			  save

			  common /dbocom/ idope(5)

			  idum = 0
			  rdum = 0.0D+00
			  nerr = 0
			  mode = 0
			  level = 1

			  if ( igo == 0) then
			!
			!  Check validity of input data.
			!
			!  See that mdw is .gt.0. gross check only.
			!
			      if ( mdw<=0) then
			          nerr = 53
			          call xerrwv('dbocls(). mdw=(i1) must be positive.', &
			                      nerr,level,1,mdw,idum,0,rdum,rdum)
			          go to 260
			      end if
			!
			!  See that number of constraints is nonnegative.
			!
			      if ( mcon < 0) then
			          nerr = 54
			          call xerrwv('dbocls(). mcon=(i1) must be nonnegative.', &
			                      nerr,level,1,mcon,idum,0,rdum,rdum)
			          go to 260
			      end if
			!
			!  See that number of unknowns is positive.
			!
			      if ( ncols<=0) then
			          nerr = 55
			          call xerrwv( &
			       'dbocls(). ncols=(i1) the no. of variables must be positive.' &
			                      ,nerr,level,1,ncols,idum,0,rdum,rdum)
			          go to 260
			      end if
			!
			!  See that constraint indicators are all well-defined.
			!
			      do j = 1,ncols + mcon
			          if ( ind(j) < 1 .or. ind(j) > 4) then
			              nerr = 56
			              call xerrwv( &
			                    'dbocls(). for j=(i1), ind(j)=(i2) must be 1-4.' &
			                          ,nerr,level,2,j,ind(j),0,rdum,rdum)
			              go to 260
			          end if
			      end do
			!
			!  See that bounds are consistent.
			!
			      do j = 1,ncols + mcon
			          if ( ind(j) == 3) then
			              if ( bl(j) > bu(j)) then
			                  nerr = 57
			                  call xerrwv( &
			        'dbocls(). for j=(i1), bound bl(j)=(r1) is  >  bu(j)=(r2).' &
			         ,nerr,level,1,j,idum,2,bl(j),bu(j))
			                  go to 260
			              end if
			          end if
			      end do
			!
			!  Process option array.
			!
			      drelpr = epsilon ( drelpr )
			      checkl = .false.
			      filter = .true.
			      lenx = 2* (ncols+mcon) + 2
			      iscale = 1
			      igo = 1
			      accum = .false.
			      pretri = .true.
			      lopt = 0
			      lp = 0
			      lds = 0

			   30     continue
			      lp = lp + lds
			      ip = iopt(lp+1)
			      jp = abs(ip)
			!
			!  Test for no more options to change.
			!
			      if ( ip == 99) then
			          if ( lopt == 0) lopt = lp+1
			!
			!  Send column scaling to DBOLS().
			!
			          idope(4)=1
			!
			!  Note that DBOLS() was called by DBOCLS()
			!
			          idope(5)=1
			!
			!  Change pretriangularization factor in DBOLSM().
			!
			          idope(1) = ncols + mcon + 1
			!
			!  Pass weight to DBOLSM() for rank test.
			!
			          idope(2) = ncols + mcon + 2
			          idope(3) = mcon
			          go to 50
			      else if ( jp == 99) then
			          lds = 1
			          go to 50
			      else if ( jp == 1) then
			          if ( ip > 0) then
			!
			!  Set up direction flag location, row stacking pointer
			!  location, and location for number of new rows.
			!
			              locacc = lp + 2
			!
			!                  IOPT(LOCACC-1)=option number for seq. accumulation.
			!     Contents..   IOPT(LOCACC  )=user direction flag, 1 or 2.
			!                  IOPT(LOCACC+1)=row stacking pointer.
			!                  IOPT(LOCACC+2)=number of new rows to process.
			!
			!     User action with this option..
			!
			!      (Set up option data for SEQ. Accumulation in IOPT(*).)
			!      (Move block of equations into W(*,*)  starting at first
			!       row of W(*,*) below the rows for the constraint matrix C.
			!       Set IOPT(LOCACC+2)=no. of least squares equations in block.
			!
			!              LOOP
			!              CALL DBOCLS()
			!
			!                  IF(IOPT(LOCACC) .EQ. 1) THEN
			!                      STACK EQUAS. INTO W(*,*), STARTING AT
			!                      ROW IOPT(LOCACC+1).
			!                       INTO W(*,*).
			!                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
			!                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
			!                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
			!                      (PROCESS IS OVER. EXIT LOOP.)
			!                  ELSE
			!                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
			!                  END IF
			!              END LOOP
			!
			              iopt(locacc+1) = mcon + 1
			              accum = .true.
			              iopt(locacc) = igo
			          end if
			          lds = 4
			          go to 30
			      else if ( jp == 2) then
			          if ( ip > 0) then
			!
			!  GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
			!
			              locdim = lp + 2
			!
			!  LMDW.GE.MCON+MAX(MOUT,NCOLS), IF MCON.GT.0 .AND FILTER
			!  LMDW.GE.MCON+MOUT, OTHERWISE
			!
			!  LNDW.GE.NCOLS+MCON+1
			!  LLB .GE.NCOLS+MCON
			!  LLX .GE.2*(NCOLS+MCON)+2+EXTRA REQD. IN OPTIONS.
			!  LLRW.GE.6*NCOLS+5*MCON
			!  LLIW.GE.2*(NCOLS+MCON)
			!  LIOP.GE. AMOUNT REQD. FOR OPTION ARRAY.
			!
			              lmdw = iopt(locdim)
			              lndw = iopt(locdim+1)
			              llb = iopt(locdim+2)
			              llx = iopt(locdim+3)
			              llrw = iopt(locdim+4)
			              lliw = iopt(locdim+5)
			              liopt = iopt(locdim+6)
			              checkl = .true.
			          end if
			          lds = 8
			          go to 30
			!
			!  OPTION TO MODIFY THE COLUMN SCALING.
			!
			      else if ( jp == 3) then
			          if ( ip > 0) then
			              iscale = iopt(lp+2)
			!
			!     SEE THAT ISCALE IS 1 THRU 3.
			!
			              if ( iscale < 1 .or. iscale > 3) then
			                  nerr = 48
			                  call xerrwv('dbocls(). iscale option=(i1) must be 1-3.' &
			                    ,nerr,level,1,iscale,idum,0,rdum,rdum)
			                  go to 260
			              end if
			          end if
			          lds = 2
			          go to 30
			!
			!  IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
			!  SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
			!
			      else if ( jp == 4) then
			          if ( ip > 0) then
			              iscale = 4
			              if ( iopt(lp+2)<=0) then
			                  nerr = 49
			                  call xerrwv('dbocls(). offset past x(ncols) (i1) for' // &
			                    'user-provided column scaling must be positive.', &
			                    nerr,level,1,iopt(lp+2),idum,0,rdum,rdum)
			                  go to 260
			              end if
			              call dcopy(ncols,x(ncols+iopt(lp+2)),1,rw,1)
			              lenx = lenx + ncols
			              do j = 1,ncols
			                  if ( rw(j)<=0.0D+00 ) then
			                      nerr = 50
			                      call xerrwv('dbocls(). each provided col. scale ' // &
			                        'factor must be positive. comp. (i1)   now = (r1).', &
			                        nerr,level,1,j,idum,1,rw(j),rdum)
			                      go to 260
			                  end if
			              end do
			          end if
			          lds = 2
			          go to 30
			!
			!  IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLS().
			!
			      else if ( jp == 5) then
			          if ( ip > 0) then
			              lopt = iopt(lp+2)
			          end if
			          lds = 2
			          go to 30
			!
			!  IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLSM().
			!  (NO LONGER USED.) OPTION NOW MUST BE PASSED IMBEDDED IN
			!  OPTION ARRAY FOR DBOLS().
			!
			      else if ( jp == 6) then
			          lds = 2
			          go to 30
			!
			!  THIS OPTION USES THE NEXT LOC OF IOPT(*) AS A
			!  POINTER VALUE TO SKIP TO NEXT.
			!
			      else if ( jp == 7) then
			          if ( ip > 0) then
			              lp = iopt(lp+2)-1
			              lds = 0
			          else
			              lds = 2
			          end if
			          go to 30
			!
			!  THIS OPTION AVOIDS THE CONSTRAINT RESOLVING PHASE FOR
			!  THE LINEAR CONSTRAINTS C*X=Y.
			!
			      else if ( jp == 8) then
			          filter = .not. (ip > 0)
			          lds = 1
			          go to 30
			!
			!  THIS OPTION SUPPRESSES PRETRIANGULARIZATION OF THE LEAST
			!  SQUARES EQATIONS.
			!
			      else if ( jp == 9) then
			          pretri = .not. (ip > 0)
			          lds = 1
			          go to 30
			!
			!  NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
			!
			      else
			          nerr = 51
			          call xerrwv('dbocls(). the option number=(i1) is not defined.' &
			            ,nerr,level,1,jp,idum,0,rdum,rdum)
			          go to 260
			      end if

			   50     continue

			      if ( checkl) then
			!
			!  CHECK LENGTHS OF ARRAYS
			!
			!  THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
			!  ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
			!
			       if ( filter .and. .not.accum) then
			         mdwl=mcon+max(mrows,ncols)
			       else if ( accum) then
			         mdwl=mcon+ncols+1
			       else
			         mdwl=mcon+ncols
			       end if

			          if ( lmdw < mdwl) then
			              nerr = 41
			              call xerrwv('dbocls(). the row dimension of w(,)=(i1) ' // &
			                'must be >= the number of effective rows=(i2).', &
			                nerr,level,2,lmdw,mdwl,0,rdum,rdum)
			              go to 260
			          end if
			          if ( lndw < ncols+mcon+1) then
			              nerr = 42
			              call xerrwv('dbocls(). the column dimension of w(,)=(i1) ' // &
			                'must be >= ncols+mcon+1=(i2).',nerr,level,2,lndw, &
			                ncols+mcon+1,0,rdum,rdum)
			              go to 260
			          end if
			          if ( llb < ncols+mcon) then
			              nerr = 43
			              call xerrwv('dbocls(). the dimensions of the arrays bl(),' // &
			                'bu(), and ind()=(i1) must be >= ncols+mcon=(i2).', &
			                nerr,level,2,llb,ncols+mcon,0,rdum,rdum)
			              go to 260
			          end if

			          if ( llx < lenx) then
			              nerr = 44
			              call xerrwv( 'dbocls(). the dimension of x()=(i1) must be ' // &
			                '>= the reqd. length=(i2).',nerr,level,2,llx,lenx, &
			                0,rdum,rdum)
			              go to 260
			          end if

			          if ( llrw < 6*ncols+5*mcon) then
			              nerr = 45
			              call xerrwv('dbocls(). the dimension of rw()=(i1) must be ' // &
			                '>= 6*ncols+5*mcon=(i2).',nerr,level,2, &
			                llrw,6*ncols+5*mcon,0,rdum,rdum)
			              go to 260
			          end if

			          if ( lliw < 2*ncols+2*mcon) then
			              nerr = 46
			              call xerrwv('dbocls() the dimension of iw()=(i1) must be ' // &
			                '>= 2*ncols+2*mcon=(i2).',nerr,level,2,lliw, &
			                2*ncols+2*mcon,0,rdum,rdum)
			              go to 260
			          end if

			          if ( liopt < lp+17) then
			              nerr = 47
			              call xerrwv('dbocls(). the dimension of iopt()=(i1) must be ' // &
			                '>= the reqd. len.=(i2).',nerr,level,2,liopt,lp+17, 0,rdum,rdum)
			              go to 260
			          end if
			      end if
			  end if
			!
			!  Optionally go back to the user for accumulation of least squares
			!  equations and directions for processing these equations.
			!
			!  Accumulate least squares equations.
			!
			  if ( accum) then
			      mrows = iopt(locacc+1) - 1 - mcon
			      inrows = iopt(locacc+2)
			      mnew = mrows + inrows
			      if ( mnew < 0 .or. mnew+mcon > mdw) then
			          nerr = 52
			          call xerrwv('dbocls(). no. of rows=(i1) must be >= 0 ' // &
			            '.and. <=mdw-mcon=(i2)',nerr,level,2,mnew,mdw-mcon,0,rdum,rdum)
			          go to 260
			      end if
			  end if
			!
			!  USE THE SOFTWARE OF DBOLS( ) FOR THE TRIANGULARIZATION OF THE
			!  LEAST SQUARES MATRIX.  THIS MAY INVOLVE A SYSTALTIC INTERCHANGE
			!  OF PROCESSING POINTERS BETWEEN THE CALLING AND CALLED (DBOLS())
			!  PROGRAM UNITS.
			!
			  jopt(01) = 1
			  jopt(02) = 2
			  jopt(04) = mrows
			  jopt(05) = 99
			  irw = ncols + 1
			  iiw = 1
			  if ( accum .or. pretri) then
			!
			!  NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
			!
			          idope(5)=0
			      call dbols(w(mcon+1,1),mdw,mout,ncols,bl,bu,ind,jopt,x,rnorm, &
			             mode,rw(irw),iw(iiw))
			  else
			      mout = mrows
			  end if

			  if ( accum) then
			    accum = iopt(locacc)  ==  1
			    iopt(locacc+1) = jopt(03) + mcon
			    mrows = min(ncols+1,mnew)
			  end if

			  if ( accum) return
			!
			!  SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM
			!
			!  MOVE RIGHT HAND SIDE OF LEAST SQUARES EQUATIONS.
			!
			  call dcopy(mout,w(mcon+1,ncols+1),1,w(mcon+1,ncols+mcon+1),1)
			  if ( mcon > 0 .and. filter) then
			!
			!  PROJECT THE LINEAR CONSTRAINTS INTO A REACHABLE SET.
			!
			      do i = 1,mcon
			        call dcopy(ncols,w(i,1),mdw,w(mcon+1,ncols+i),1)
			      end do
			!
			!  PLACE (-)IDENTITY MATRIX AFTER CONSTRAINT DATA.
			!
			      do j = ncols + 1, ncols + mcon + 1
			        w(1:mcon,j) = 0.0D+00
			      end do

			      w(1:mcon,ncols+1) = -1.0D+00
			!
			!  OBTAIN A 'FEASIBLE POINT' FOR THE LINEAR CONSTRAINTS.
			!
			      jopt(01) = 99
			      irw = ncols + 1
			      iiw = 1
			!
			!  NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
			!
			          idope(5)=0
			      call dbols(w,mdw,mcon,ncols+mcon,bl,bu,ind,jopt,x,rnormc, &
			        modec,rw(irw),iw(iiw))
			!
			!  ENLARGE THE BOUNDS SET, IF REQUIRED, TO INCLUDE POINTS THAT
			!  CAN BE REACHED.
			!
			      do j = ncols + 1,ncols + mcon
			          icase = ind(j)
			          if ( icase < 4) then
			              t = ddot ( ncols, w(mcon+1,j), 1, x, 1 )
			          end if
			          go to (80,90,100,110),icase
			          go to 120
			   80         bl(j) = min(t,bl(j))
			          go to 120
			   90         bu(j) = max(t,bu(j))
			          go to 120
			  100         bl(j) = min(t,bl(j))
			          bu(j) = max(t,bu(j))
			          go to 120
			  110         continue
			  120         continue
			      end do
			!
			!  MOVE CONSTRAINT DATA BACK TO THE ORIGINAL AREA.
			!
			      do j = ncols + 1,ncols + mcon
			          call dcopy(ncols,w(mcon+1,j),1,w(j-ncols,1),mdw)
			      end do

			  end if

			  if ( mcon > 0) then
			      do j = ncols + 1,ncols + mcon
			        w(mcon+1:mcon+mout,j) = 0.0D+00
			      end do
			!
			!  PUT IN (-)IDENTITY MATRIX (POSSIBLY) ONCE AGAIN.
			!
			      do j = ncols + 1,ncols + mcon + 1
			        w(1:mcon,j) = 0.0D+00
			      end do

			      w(1:mcon,ncols+1) = -1.0D+00

			  end if
			!
			!  COMPUTE NOMINAL COLUMN SCALING FOR THE UNWEIGHTED MATRIX.
			!
			  cnorm = 0.0D+00
			  anorm = 0.0D+00
			  do j = 1,ncols
			      t1 = dasum(mcon,w(1,j),1)
			      t2 = dasum(mout,w(mcon+1,1),1)
			      t = t1 + t2
			      if ( t == 0.0D+00 ) t = one
			      cnorm = max(cnorm,t1)
			      anorm = max(anorm,t2)
			      x(ncols+mcon+j) = one/t
			  end do

			  go to (180,190,210,220),iscale
			  go to 230
			  180 continue
			  go to 230
			!
			!  SCALE COLS. (BEFORE WEIGHTING) TO HAVE LENGTH ONE.
			!
			  190 continue

			  do j = 1,ncols
			    t = dnrm2(mcon+mout,w(1,j),1)
			    if ( t == 0.0D+00 ) t = one
			    x(ncols+mcon+j) = one/t
			  end do

			  go to 230
			!
			!  SUPPRESS SCALING (USE UNIT MATRIX).
			!
			  210 continue

			  x(ncols+mcon+1:ncols+mcon+ncols) = one

			  go to 230
			!
			!  THE USER HAS PROVIDED SCALING.
			!
			  220 call dcopy(ncols,rw,1,x(ncols+mcon+1),1)
			  230 continue

			  do j = ncols + 1,ncols + mcon
			    x(ncols+mcon+j) = one
			  end do
			!
			!  WEIGHT THE LEAST SQUARES EQUATIONS.
			!
			  wt = sqrt(drelpr)
			  if ( anorm > 0.0D+00 ) wt = wt/anorm
			  if ( cnorm > 0.0D+00 ) wt = wt*cnorm

			  do i = 1,mout
			      call dscal(ncols,wt,w(i+mcon,1),mdw)
			  end do
			  call dscal(mout,wt,w(mcon+1,mcon+ncols+1),1)
			  lrw = 1
			  liw = 1
			!
			!  SET THE NEW TRIANGULARIZATION FACTOR.
			!
			  x(ncols+mcon+idope(1))= 0.0D+00
			!
			!  SET THE WEIGHT TO USE IN COMPONENTS .GT. MCON,
			!  WHEN MAKING LINEAR INDEPENDENCE TEST.
			!
			  x(ncols+mcon+idope(2))= one/wt
			  idope(5)=1
			  call dbols(w,mdw,mout+mcon,ncols+mcon,bl,bu,ind,iopt(lopt),x, &
			    rnorm,mode,rw(lrw),iw(liw))

			  260 continue

			  if ( 0 <= mode ) then
			    mode = -nerr
			  end if

			  igo = 0

			  return*/
}