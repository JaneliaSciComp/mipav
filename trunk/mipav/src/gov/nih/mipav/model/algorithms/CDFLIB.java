package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.Preferences;

// Ported from a site at the University of Texas Department of Biostatistics and Applied Mathematics
// which makes available a FORTRAN90 version of CDFLIB.
/** Legalities
==========

We place our efforts in writing this package in the public domain.
However, code from ACM publications is subject to the ACM policy
(below).
     
References
==========
      
     
Incomplete Beta
--------------- 

DiDinato,  A.    R.   and  Morris,  A.  H.   (1993)  ``Algorithm  708:
Significant  Digit   Computation  of  the   Incomplete  Beta  Function
Ratios.''  ACM Trans.  Math.  Softw. 18, 360-373.
     
Incomplete Gamma
----------------

DiDinato,  A.  R.   and Morris,  A.  H.  (1986)  ``Computation of  the
incomplete gamma function ratios and their inverse.'' ACM Trans. Math.
Softw. 12, 377-393.
     
Cumulative Normal
-----------------

Cody,  W.D. (1993).  ``ALGORITHM  715: SPECFUN  -  A Portable  FORTRAN
Package  of   Special  Function   Routines  and  Test   Drivers''  ACM
Trans. Math.  Softw. 19, 22-32.
     
Inverse Normal
--------------

``Algorithm AS241'' (1988) Appl. Statist. 37, NO. 3, 477-484.
     
Finding a Zero of a Monotone Function
-------------------------------------

Alefeld,  G.  E.,  Potra,  F.  A., Shi,  Y.  (1995)  ``Algorithm  748:
Enclosing Zeros  of Continuous Functions.'',  by G. E. Alefeld,  F. A.
Potra, YiXun Shi, ACM Trans. Math. Softw., 21, No. 3, 327-344
     
ACM Policy on Use of Code
=========================
Here is the software Policy of the ACM.

Submittal of an algorithm  for publication in one of the
ACM  Transactions implies that  unrestricted use  of the
algorithm  within  a  computer is  permissible.  General
permission to copy  and distribute the algorithm without
fee is granted provided that  the copies are not made or
distributed  for direct  commercial  advantage. The  ACM
copyright notice  and the  title of the  publication and
its date appear, and notice  is given that copying is by
permission    of   the    Association    for   Computing
Machinery. To copy  otherwise, or to republish, requires
a  fee  and/or  specific  permission. Krogh,  F.  (1997)
``Algorithms  Policy.''  ACM  Tran.  Math.   Softw.  13,
183-186.

We do not know the policy  of the Royal Statistical Society; they have
discontinued  publishing algorithms.  However, they  made a  number of
these  programs available  on Statlib  on condition  that there  be no
charge for their distribution.

Here is our standard disclaimer.

      NO WARRANTY

WE  PROVIDE ABSOLUTELY  NO WARRANTY  OF ANY  KIND EITHER  EXPRESSED OR
IMPLIED,  INCLUDING BUT  NOT  LIMITED TO,  THE  IMPLIED WARRANTIES  OF
MERCHANTABILITY AND FITNESS FOR  A PARTICULAR PURPOSE. THE ENTIRE RISK
AS TO THE  QUALITY AND PERFORMANCE OF THE PROGRAM  IS WITH YOU. SHOULD
THIS PROGRAM  PROVE DEFECTIVE,  YOU ASSUME THE  COST OF  ALL NECESSARY
SERVICING, REPAIR  OR CORRECTION.IN NO  EVENT SHALL THE  UNIVERSITY OF
TEXAS OR  ANY OF ITS  COMPONENT INSTITUTIONS INCLUDING M.  D. ANDERSON
HOSPITAL BE  LIABLE TO  YOU FOR DAMAGES,  INCLUDING ANY  LOST PROFITS,
LOST  MONIES, OR  OTHER SPECIAL,  INCIDENTAL OR  CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OR  INABILITY TO USE (INCLUDING BUT NOT LIMITED
TO LOSS OF  DATA OR DATA OR ITS ANALYSIS  BEING RENDERED INACCURATE OR
LOSSES  SUSTAINED BY  THIRD  PARTIES) THE  PROGRAM.
*/


public class CDFLIB {
	 /*----------------------------------------------------------------------

	 !                                cdf_f_mod
	 !                                *=*=*=*=*

	 !  -  SUBROUTINE CDF_F( WHICH, CUM, CCUM, F, DFN, DFD, STATUS, CHECK_INPUT)
	 !  -  REAL (dpkind) FUNCTION CUM_F(F,  DFN, DFD, STATUS, CHECK_INPUT)
	 !  -  REAL (dpkind) FUNCTION CCUM_F(F, DFN, DFD, STATUS,  CHECK_INPUT)
	 !  -  REAL (dpkind) FUNCTION INV_F( CUM, CCUM, DFN, DFD, STATUS, CHECK_INPUT)

	 !                             The Distribution
	 !                             ================

	 !  F is the distribution of the ratio of two independent random
	 !  variables. The numerator random variable is distributed as chi-squared
	 !  with DF degrees of freedom divided by DF. The denominator random
	 !  variable is distributed as chi-squared with DFD degrees of freedom
	 !  divided by DFD.The density of the f distribution is defined on x in
	 !  [0,+Infinity] and is proportional to:

	 !                                (DFN-2)/2)        
	 !                               x                  
	 !                        ------------------------- 
	 !                                      (DFN+DFD)/2 
	 !                        [1+(DFN/DFD)x]            

	 !                                Arguments
	 !                                =========

	 !  - INTEGER, INTENT(IN) :: WHICH. Integer indicating which of the next
	 !    four arguments is to be calculated.
	 !  Input Range: [ 1:2 ]
	 !     1.  CUM and CCUM 
	 !     2.  F 
	 !  NOTE: DFN and DFD will not be computed because CUM is not monotone in
	 !    either argument.
	 !  - REAL (dpkind), OPTIONAL :: CUM. The CDF of the f distribution.
	 !  Range: [ 0:1-10^-10 ]
	 !  - REAL (dpkind), OPTIONAL :: CCUM. One minus the CDF of the f
	 !    distribution.
	 !  Range: [ 10^-10:1 ]
	 !  - REAL (dpkind) :: F. The upper limit of integration of the f density.
	 !    The lower limit is 0.
	 !  Input Range: [ 0:10^100 ]
	 !  - REAL (dpkind) :: DFN. The numerator degrees of freedom.
	 !  Range: [ 10^-3:10^10 ]
	 !  - REAL (dpkind) :: DFD. The denominator degrees of freedom.
	 !  Range: [ 10^-3:10^10 ]
	 !  - INTEGER, OPTIONAL, INTENT(OUT) :: STATUS. Return code. 
	 !     -1 WHICH outside input range 
	 !     -2 CUM outside range 
	 !     -3 CCUM outside range 
	 !     -4 F outside range 
	 !     -5 DFN outside range 
	 !     -6 DFD outside range 
	 !      3 CUM + CCUM is not nearly one 
	 !     10 can not solve cdf_beta calling from local_cum_f
	 !    -50 Answer (if any) is BELOW the LOWER search bound 
	 !     50 Answer (if any) is ABOVE the UPPER search bound 
	 !  - LOGICAL, INTENT(IN), OPTIONAL :: CHECK_INPUT. If PRESENT and
	 !       .TRUE. input argument values are not checked for validity.

	 !   NOTE: CUM and CCUM must add to (nearly) one.

	 !   NOTE: The value of the CDF of the f distribution is NOT necessarily
	 !         monotone in either degree of freedom argument. There may thus
	 !         be two values that provide a given DCF value. 
	 !         This routine assumes monotonicity and will find an arbitrary one
	 !         of the two values.
	 */
    
     private final double tiny = 1.0E-100;
     
     public double cum_f(double f,double dfn,double dfd,int status[], boolean check_input) {
    	     int which = 1;
    	     double cum[] = new double[]{1};
    	     double ccum[] = new double[]{0};
             cdf_f(which,cum,ccum,f,dfn,dfd,status,check_input);
             return cum[0];
     }
     
     private void cdf_f(int which, double cum[], double ccum[] ,
    		 double f, double dfn, double dfd,int status[], boolean check_input) {
    	 boolean match_cum;
    	 double local_cum[] = new double[1];
    	 double local_ccum[] = new double[1];
     /*! .. Use Statements ..
             USE cdf_aux_mod
             USE zero_finder
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), OPTIONAL :: ccum, cum
             REAL (dpkind) :: dfd, dfn, f
             INTEGER, OPTIONAL, INTENT (OUT) :: status
             INTEGER, INTENT (IN) :: which
             LOGICAL, OPTIONAL, INTENT (IN) :: check_input
     ! ..
     ! .. Local Structures ..
             TYPE (zf_locals) :: local
     ! ..
     ! .. Local Arrays ..
             REAL (dpkind) :: params(6)
     ! ..
     ! .. Local Scalars ..
             REAL (dpkind) :: fx, local_ccum, local_cum, try_ccum, try_cum
             INTEGER :: zf_status
             LOGICAL :: has_status, local_check_input, match_cum
     ! ..
     ! .. Intrinsic Functions ..
             INTRINSIC PRESENT
     ! ..
             has_status = PRESENT(status)

     ! status = 0 means NO error

             IF (has_status) THEN
               status = 0
             END IF

             CALL check_complements(cum,ccum,the_f%name,'cum','ccum', &
               local_cum,local_ccum,set_values=(which/=1),bad_status=3, &
               status=status)

             IF (has_status) THEN
               IF (status/=0) THEN
                 RETURN
               END IF
             END IF

             params(1) = local_cum
             params(2) = local_ccum
             params(3) = f
             params(4) = dfn
             params(5) = dfd

             IF (PRESENT(check_input)) THEN
               local_check_input = check_input
             ELSE
               local_check_input = .TRUE.
             END IF

             IF (local_check_input) THEN
               CALL validate_parameters(the_f,which,params,status)

               IF (has_status) THEN
                 IF (status/=0) THEN
                   RETURN
                 END IF
               END IF
             END IF

     ! ++++++++++          ++++++++++          ++++++++++
     ! Compute the Answers
     ! ++++++++++          ++++++++++          ++++++++++*/

             if (which>1) match_cum = (local_cum[0]<=0.5);

             switch (which) {
             case 1:

                local_cum_f(f,dfn,dfd,local_cum,local_ccum,status, check_input);

                if (cum[0] == 1) cum[0] = local_cum[0];
                if (ccum[0] == 1) ccum[0] = local_ccum[0];
                break;

             case 2:
               /*f = five
               zf_status = 0

               CALL cdf_set_zero_finder(the_f,3,local)

               DO
                 CALL rc_interval_zf(zf_status,f,fx,local)

                 IF (zf_status/=1) EXIT

                 CALL local_cum_f(f,dfn,dfd,try_cum,try_ccum,status)

                 IF (has_status) THEN
                   IF (status/=0) THEN
                     RETURN
                   END IF
                 END IF

                 IF (match_cum) THEN
                   fx = try_cum - local_cum
                 ELSE
                   fx = try_ccum - local_ccum
                 END IF
               END DO

     ! Can NOT solve for Degrees of Freedom

             END SELECT*/
            	 break;
             } // switch(which)

             /*IF (has_status) THEN
     ! Set the status of the zero finder
               CALL cdf_finalize_status(local,status)
             END IF*/

             return;

     }
     
     private void local_cum_f(double f,double dfn,double dfd,double cum[],double ccum[],int status[], boolean check_input) {
     /*----------------------------------------------------------------------

     !                              Function

     !     Computes the integral from 0 to F of the f-density.
     !     f    --> Upper limit of integration of the f-density.
     !     dfn  --> Degrees of freedom of the numerator
     !     dfd  --> Degrees of freedom of the denominator
     !     cum  <-- Cumulative f-distribution.
     !     ccum <-- Complement of Cumulative f-distribution.
     !     status <-- tells the caller whether the problem was successfully solved.
     !                Set to 10 if cdf_beta has no answer and status was used in
     !                the initial call to cdf_f (otherwise, it breaks down)

     !                              Method

     !     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of
     !     Mathematical  Functions (1966) is used to reduce the computation
     !     of the  cumulative  distribution function for the  F  variate to
     !     that of an incomplete beta.

     !                              Note
     !     If F is less than or equal to 0, 0 is returned.
     !------------------------------------------------------------------
     ! .. Use Statements ..
             USE cdf_beta_mod
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (OUT) :: ccum, cum
             REAL (dpkind), INTENT (IN) :: dfd, dfn, f
             INTEGER, OPTIONAL, INTENT (OUT) :: status
     ! ..
     ! .. Local Scalars ..
             REAL (dpkind) :: dsum, prod, xx, yy
             INTEGER :: beta_status
     ! ..
     ! .. Intrinsic Functions ..
             INTRINSIC PRESENT
     ! ..
     */
    	 int beta_status[] = new int[]{0};
    	 double prod;
    	 double dsum;
    	 double xx;
    	 double yy;
             if (f<=0.0) {
               cum[0] = 0.0;
               ccum[0] = 1.0;
               return;
             }


             prod = dfn*f;

     /*     XX is such that the incomplete beta with parameters
     !     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM

     !     YY is 1 - XX

     !     Calculate the smaller of XX and YY accurately*/

             dsum = dfd + prod;

             //IF (xx>half) THEN
             //  yy = prod/dsum
             //  xx = one - yy
            // ELSE
               xx = dfd/dsum;
               yy = 1.0 - xx;
             //END IF

             cdf_beta(1,ccum,cum,xx,yy,0.5*dfd,0.5*dfn, beta_status, check_input);

             if (beta_status[0] != 0) {
               // cdf_beta has NO answer.

               status[0] = 10;
               Preferences.debug("Error in local_cum_f call to cdf_beta\n", Preferences.DEBUG_ALGORITHM);
               Preferences.debug("Status: " +  beta_status[0] + "\n", Preferences.DEBUG_ALGORITHM);

             }

             return;

     }
     
     /*----------------------------------------------------------------------

     !                               cdf_beta_mod
     !                               *=*=*=*=*=*=

     !  -  SUBROUTINE CDF_BETA(WHICH, CUM, CCUM, X, CX, A, B, STATUS, CHECK_INPUT)
     !  -  REAL (dpkind) FUNCTION CUM_BETA(X, A, B, STATUS, CHECK_INPUT)
     !  -  REAL (dpkind) FUNCTION CCUM_BETA(X, A, B, STATUS, CHECK_INPUT)
     !  -  REAL (dpkind) FUNCTION INV_BETA(CUM, CCUM, A, B, STATUS, CHECK_INPUT)

     !                             The Distribution
     !                             ================

     !    The density of the beta distribution is defined on x in [0,1] and is
     !    proportional to:

     !                                 a      b
     !                                x  (1-x)

     !                                Arguments
     !                                =========

     !  - INTEGER, INTENT(IN) :: WHICH. Integer indicating which of the next
     !    four arguments is to be calculated.
     !  Input Range: [ 1:4 ]
     !     1.  CUM and CCUM
     !     2.  X and CX
     !     3.  A
     !     4.  B
     !  - REAL (dpkind), OPTIONAL :: CUM. The CDF of the beta distribution.
     !  Range: [ 0:1 ]
     !  - REAL (dpkind), OPTIONAL :: CCUM. One minus the CDF of the beta
     !    distribution.
     !  Range: [ 0:1 ]
     !  - REAL (dpkind), OPTIONAL :: X. The upper limit of integration of the
     !    beta density. The lower limit is 0.
     !  Range: [ 0:1 ]
     !  - REAL (dpkind), OPTIONAL :: CX. One minus the upper limit of
     !    integration of the beta density. The lower limit is 0.
     !  Range: [ 0:1 ]
     !  - REAL (dpkind) :: A. The first parameter of the beta density.
     !  Range: [ 10^-10:10^10 ]
     !  - REAL (dpkind) :: B. The second parameter of the beta density.
     !  Range: [ 10^-10:10^10 ]
     !  - INTEGER, OPTIONAL, INTENT(OUT) :: STATUS. Return code.  Possible values:
     !      0 problem solved successfully
     !     -1 WHICH outside input range
     !     -2 CUM outside range
     !     -3 CCUM outside range
     !     -4 X outside range
     !     -5 CX outside range
     !     -6 A outside range
     !     -7 B outside range
     !      3 CUM + CCUM is not nearly one
     !      4 X + CX is not nearly one
     !    -50 Answer (if any) is BELOW the LOWER search bound
     !     50 Answer (if any) is ABOVE the UPPER search bound
     !  - LOGICAL, INTENT(IN), OPTIONAL :: CHECK_INPUT. If PRESENT and
     !       .TRUE. input argument values are not checked for validity.

     !  NOTE: CUM and CCUM and also X and CX must add to (nearly) one.*/
     
     private void cdf_beta(int which,double cum[], double ccum[], double x,
    		 double cx, double a,double b, int status[], boolean check_input) {
    	 
       boolean match_cum;
       double local_cum[] = new double[1];
       double local_ccum[] = new double[1];
       double local_x = x;
       double local_cx = cx;
    /* ! .. Use Statements ..
             USE cdf_aux_mod
             USE zero_finder
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind) :: a, b
             REAL (dpkind), OPTIONAL :: ccum, cum, cx, x
             INTEGER, OPTIONAL, INTENT (OUT) :: status
             INTEGER, INTENT (IN) :: which
             LOGICAL, OPTIONAL, INTENT (IN) :: check_input
     ! ..
     ! .. Local Structures ..
     ! .. Local Arrays
             TYPE (zf_locals) :: local
     ! ..
     ! .. Local Scalars ..
             REAL (dpkind) :: fx, local_ccum, local_cum, local_cx, local_x, &
               try_ccum, try_cum
             INTEGER :: zf_status
             LOGICAL :: has_status, local_check_input, match_cum
     ! ..
     ! .. Intrinsic Functions ..
             INTRINSIC PRESENT
     ! ..
     ! .. Local Arrays ..
             REAL (dpkind) :: params(6)
     ! ..
             has_status = PRESENT(status)

     ! status = 0 means no error

             IF (has_status) THEN
               status = 0
             END IF

             CALL check_complements(cum,ccum,the_beta%name,'cum','ccum', &
               local_cum,local_ccum,set_values=(which/=1),bad_status=3, &
               status=status)

             IF (has_status) THEN
               IF (status/=0) THEN
                 RETURN
               END IF
             END IF

             CALL check_complements(x,cx,the_beta%name,'x','cx',local_x, &
               local_cx,set_values=(which/=2),bad_status=4,status=status)

             IF (has_status) THEN
               IF (status/=0) THEN
                 RETURN
               END IF
             END IF

             params(1) = local_cum
             params(2) = local_ccum
             params(3) = local_x
             params(4) = local_cx
             params(5) = a
             params(6) = b

             IF (PRESENT(check_input)) THEN
               local_check_input = check_input
             ELSE
               local_check_input = .TRUE.
             END IF

     ! ++++++++++          ++++++++++          ++++++++++
     ! Range check arguments and see that they add to one
     ! ++++++++++          ++++++++++          ++++++++++

             IF (local_check_input) THEN
     ! Assure that x + cx nearly one

               IF (which/=2) THEN
                 IF ( .NOT. add_to_one(local_x,local_cx,the_beta%name,'x','cx' &
                   ,4,status)) RETURN
               END IF

               CALL validate_parameters(the_beta,which,params,status)

               IF (has_status) THEN
                 IF (status/=0) THEN
                   RETURN
                 END IF
               END IF

             END IF

     !++++++++++++++++++++++++++++++++++++++++++++++++++
     ! Compute the Answers
     !++++++++++++++++++++++++++++++++++++++++++++++++++
*/
             if (which>1) match_cum = (local_cum[0]<=0.5);

             switch (which) {

             case 1:
            //  Calculate cum and ccum

               local_cum_beta(local_x,local_cx,a,b,local_cum,local_ccum);

               if (cum[0] == 1) cum[0] = local_cum[0];
               if (ccum[0] == 1) ccum[0] = local_ccum[0];

               return;

            /* CASE (2)
     ! Calculate x and cx

               local_x = half
               zf_status = 0

               CALL cdf_set_zero_finder(the_beta,3,local)

               IF (match_cum) THEN
                 DO
                   CALL rc_interval_zf(zf_status,local_x,fx,local)

                   IF (zf_status/=1) EXIT

                   local_cx = one - local_x

                   CALL local_cum_beta(local_x,local_cx,a,b,try_cum,try_ccum)

                   fx = try_cum - cum
                 END DO

               ELSE
                 DO
                   CALL rc_interval_zf(zf_status,local_cx,fx,local)

                   IF (zf_status/=1) EXIT

                   local_x = one - local_cx

                   CALL local_cum_beta(local_x,local_cx,a,b,try_cum,try_ccum)

                   fx = try_ccum - ccum
                 END DO
               END IF

               IF (PRESENT(x)) x = local_x
               IF (PRESENT(cx)) cx = local_cx

             CASE (3)
     ! Calculate a

               a = five
               zf_status = 0

               CALL cdf_set_zero_finder(the_beta,5,local)

               DO
                 CALL rc_step_zf(zf_status,a,fx,local)

                 IF (zf_status/=1) EXIT

                 CALL local_cum_beta(local_x,local_cx,a,b,try_cum,try_ccum)

                 IF (match_cum) THEN
                   fx = try_cum - cum
                 ELSE
                   fx = try_ccum - ccum
                 END IF
               END DO

             CASE (4)
     ! Calculate b

               b = five
               zf_status = 0

               CALL cdf_set_zero_finder(the_beta,6,local)

               DO
                 CALL rc_step_zf(zf_status,b,fx,local)

                 IF (zf_status/=1) EXIT

                 CALL local_cum_beta(local_x,local_cx,a,b,try_cum,try_ccum)

                 IF (match_cum) THEN
                   fx = try_cum - cum
                 ELSE
                   fx = try_ccum - ccum
                 END IF
               END DO

             END SELECT

             IF (has_status) THEN
               CALL cdf_finalize_status(local,status)
             END IF

             RETURN*/
             }

     }
     
     private void local_cum_beta(double x,double y,double a,double b,double cum[], double ccum[]) {
     /*----------------------------------------------------------------------
     !          Double precision cUMulative incomplete BETa distribution

     !                              Function

     !     Calculates the cdf to X of the incomplete beta distribution
     !     with parameters a and b.  This is the integral from 0 to x
     !     of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)

     !                              Arguments

     !     X --> Upper limit of integration.
     !     Y --> 1 - X.
     !     A --> First parameter of the beta distribution.
     !     B --> Second parameter of the beta distribution.
     !     CUM <-- Cumulative incomplete beta distribution.
     !     CCUM <-- Compliment of Cumulative incomplete beta distribution.

     !                              Method

     !     Cumulative distribution function  (CUM)  is calculated directly by
     !     code associated with the following reference.
     !     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
     !     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
     !     Trans. Math.  Softw. 18 (1993), 360-373.
     !     Computation of other parameters involve a seach for a value that
     !     produces  the desired  value  of CUM.   The search relies  on  the
     !     monotinicity of CUM with the other parameter.
     !----------------------------------------------------------------------
     ! .. Use Statements ..
             USE biomath_mathlib_mod
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, x, y
             REAL (dpkind), INTENT (OUT) :: ccum, cum
     ! ..
     ! .. Local Scalars ..
             INTEGER :: ierr
     ! */
    	 int ierr[] = new int[1];
             if (x<=0.0) {
               cum[0] = 0.0;
               ccum[0] = 1.0;
               return;
             }

             if (y<=0.0) {
               cum[0] = 1.0;
               ccum[0] = 0.0;
               return;
             }

     // Because of the bounds on a, b, x, y
     // ierr can not be <> 0

             bratio(a,b,x,y,cum,ccum,ierr);

             return;

     }
     
     private double EPSILON() {
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
	     return epsilon;
     }
     
     private void bratio(double a, double b,double x,double y,double w[], double w1[],int ierr[]) {
/*----------------------------------------------------------------------
!            EVALUATION OF THE INCOMPLETE BETA FUNCTION IX(A,B)
!                     --------------------
!     IT IS ASSUMED THAT A AND B ARE NONNEGATIVE, AND THAT X .LE. 1
!     AND Y = 1 - X.  BRATIO ASSIGNS W AND W1 THE VALUES
!                      W  = IX(A,B)
!                      W1 = 1 - IX(A,B)
!     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
!     IF NO INPUT ERRORS ARE DETECTED THEN IERR IS SET TO 0 AND
!     W AND W1 ARE COMPUTED. OTHERWISE, IF AN ERROR IS DETECTED,
!     THEN W AND W1 ARE ASSIGNED THE VALUE 0 AND IERR IS SET TO
!     ONE OF THE FOLLOWING VALUES ...
!        IERR = 1  IF A OR B IS NEGATIVE
!        IERR = 2  IF A = B = 0
!        IERR = 3  IF X .LT. 0 OR X .GT. 1
!        IERR = 4  IF Y .LT. 0 OR Y .GT. 1
!        IERR = 5  IF X + Y .NE. 1
!        IERR = 6  IF X = A = 0
!        IERR = 7  IF Y = B = 0
!--------------------
!     WRITTEN BY ALFRED H. MORRIS, JR.
!        NAVAL SURFACE WARFARE CENTER
!        DAHLGREN, VIRGINIA
!     REVISED ... NOV 1991
!-----------------------------------------------------------------------
! .. Scalar Arguments ..
       REAL (dpkind), INTENT (IN) :: a, b, x, y
       REAL (dpkind), INTENT (OUT) :: w, w1
       INTEGER, INTENT (OUT) :: ierr
! ..
! .. Local Scalars ..
       REAL (dpkind) :: a0, b0, eps, lambda, t, x0, y0, z
       INTEGER :: ierr1, ind, n
! ..
! .. Intrinsic Functions ..
       INTRINSIC ABS, EPSILON, MAX, MIN
! ..*/
    double eps;
    int ind;
    int n;
    double a0, b0, x0, y0;
    double lambda = 0.0;
    double t;
    int ierr1[] = new int[1];
    boolean do5 = true;
    boolean do10 = true;
    boolean do20 = true;
    boolean do30 = true;
    boolean do40 = true;
    boolean do50 = true;
    boolean do60 = true;
    boolean do70 = true;
    boolean do80 = true;
    boolean do90 = true;
    boolean do100 = true;
    boolean do110 = true;
    boolean do120 = true;
    boolean do130 = true;
    boolean do140 = true;
       eps = EPSILON();
       double z;

       w[0] = 0.0;
       w1[0] = 0.0;

       if (a< 0.0 || b< 0.0) {
         ierr[0] = 1;
         return;
       }

       if (a==0.0 && b==0.0) {
         ierr[0] = 2;
         return;
       }

       if (x<0.0 || x>1.0) {
         ierr[0] = 3;
         return;
       }

       if (y<0.0 || y>1.0) {
         ierr[0] = 4;
         return;
       }

       z = ((x+y)-0.5) - 0.5;

       if (Math.abs(z)>3.0*eps) {
         ierr[0] = 5;
         return;
       }

       ierr[0] = 0;
       if (x==0.0) {
    	  if (a==0.0) {
           ierr[0] = 6;
           return;
    	  }

         w[0] = 0.0;
         w1[0] = 1.0;
         return;   
       } // if (x==0.0)
       if (y==0.0) {
    	   if (b==0.0) {
               ierr[0] = 7;
               return;
    	   }

         w[0] = 1.0;
         w1[0] = 0.0;
         return; 
       } // if (y == 0.0)
       if (a==0.0) {
    	   w[0] = 1.0;
    	   w1[0] = 0.0;
    	   return;
       } // if (a==0.0)
       if (b==0.0) {
    	   w[0] = 0.0;
    	   w1[0] = 1.0;
    	   return; 
       } // if (b == 0.0)

       eps = Math.max(eps,1.0E-15);

       if (Math.max(a,b)<1.0E-3*eps) {
    	   // PROCEDURE FOR A AND B .LT. 1.E-3*EPS
    	   w[0] = b/(a+b);
    	   w1[0] = a/(a+b);
    	   return;
       }

       ind = 0;
       a0 = a;
       b0 = b;
       x0 = x;
       y0 = y;
       if (Math.min(a0,b0)>1.0) {
    	   do5 = false;
    	   do10 = false;
    	   do20 = false;
       }

      if (do5) {
	      if (x>0.5) {
	         ind = 1;
	         a0 = b;
	         b0 = a;
	         x0 = y;
	         y0 = x;
	      }
	
	       if (b0<Math.min(eps,eps*a0)) {
	    	   do10 = false;
	    	   do20 = false;
	    	   do30 = false;
	    	   do40 = false;
	       }
	       else if (a0<Math.min(eps,eps*b0) && b0*x0<=1.0) {
	    	   do10 = false;
	    	   do20 = false;
	    	   do30 = false;
	    	   do40 = false;
	    	   do50 = false;
	       }
	       else if (Math.max(a0,b0)>1.0) {
	    	   
	       }
	       else if (a0>=Math.min(0.2,b0)) {
	    	   do10 = false;
	    	   do20 = false;
	    	   do30 = false;
	    	   do40 = false;
	    	   do50 = false;
	    	   do60 = false;
	       }
	       else if (Math.pow(x0,a0)<=0.9) {
	    	   do10 = false;
	    	   do20 = false;
	    	   do30 = false;
	    	   do40 = false;
	    	   do50 = false;
	    	   do60 = false;
	       }
	       else if (x0>=0.3) {
	    	   do10 = false;
	    	   do20 = false;
	    	   do30 = false;
	    	   do40 = false;
	    	   do50 = false;
	    	   do60 = false;
	    	   do70 = false;
	       }
	       else {
	           n = 20;
	           do10 = false;
	    	   do20 = false;
	    	   do30 = false;
	    	   do40 = false;
	    	   do50 = false;
	    	   do60 = false;
	    	   do70 = false;
	    	   do80 = false;
	    	   do90 = false;
	       }
      } // if (do5)

      if (do10) {
       if (b0<=1.0) {
    	   do20 = false;
    	   do30 = false;
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
       }
       else if (x0>=0.3) {
    	   do20 = false;
    	   do30 = false;
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
       }
       else if (x0>=0.1) {
  
       }
       else if (Math.pow((x0*b0),a0)<=0.7) {
    	   do20 = false;
    	   do30 = false;
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
       }
      } // if (do10)

      if (do20) {
       if (b0>15.0) {
    	   do30 = false;
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
    	   do90 = false;
    	   do100 = false;
       }
       else {
           n = 20;
           do30 = false;
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
    	   do90 = false;
       }
      } // if (do20)

     if (do30) {
       if (a<=b) {
         lambda = a - (a+b)*x;
       }
       else {
         lambda = (a+b)*y - b;
       }

       if (lambda<0.0) {
         ind = 1;
         a0 = b;
         b0 = a;
         x0 = y;
         y0 = x;
         lambda = Math.abs(lambda);
       }

       if (b0<40.0 && b0*x0<=0.7) {
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
       }
       else if (b0<40.0) {
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
    	   do90 = false;
    	   do100 = false;
    	   do110 = false;
       }
       else if (a0>b0) {
    	   
       }
       else if (a0<=100.0) {
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
       }
       else if (lambda>0.03*a0) {
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
       }
       else {
    	   do40 = false;
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
    	   do90 = false;
    	   do100 = false;
    	   do110 = false;
    	   do120 = false;
    	   do130 = false;
       }
     } // if (do30)

     if (do40) {
       if (b0<=100.0) {
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
       }
       else if (lambda>0.03*b0) {
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
       }
       else {
    	   do50 = false;
    	   do60 = false;
    	   do70 = false;
    	   do80 = false;
    	   do90 = false;
    	   do100 = false;
    	   do110 = false;
    	   do120 = false;
    	   do130 = false;
       }
     } // if (do40)

//            EVALUATION OF THE APPROPRIATE ALGORITHM

    if (do50) {
       w[0] = fpser(a0,b0,x0,eps);
       w1[0] = 0.5 + (0.5-w[0]);
       do60 = false;
	   do70 = false;
	   do80 = false;
	   do90 = false;
	   do100 = false;
	   do110 = false;
	   do120 = false;
	   do130 = false;
	   do140 = false;
    } // if (do50)

    if (do60) {
       w1[0] = apser(a0,b0,x0,eps);
       w[0] = 0.5 + (0.5-w1[0]);
       do70 = false;
	   do80 = false;
	   do90 = false;
	   do100 = false;
	   do110 = false;
	   do120 = false;
	   do130 = false;
	   do140 = false;
    } // if (do60)

    /*if (do70) {
       w[0] = bpser(a0,b0,x0,eps);
       w1[0] = 0.5 + (0.5-w[0]);
       do80 = false;
	   do90 = false;
	   do100 = false;
	   do110 = false;
	   do120 = false;
	   do130 = false;
	   do140 = false;
    } // if (do70)
       

    if (do80) {
       w1[0] = bpser(b0,a0,y0,eps);
       w[0] = 0.5 + (0.5-w1[0]);
       do90 = false;
	   do100 = false;
	   do110 = false;
	   do120 = false;
	   do130 = false;
	   do140 = false;
    } // if (do80)

    if (do90) {
       w[0] = bfrac(a0,b0,x0,y0,lambda,15.0*eps);

       w1[0] = 0.5 + (0.5-w[0]);
       do100 = false;
	   do110 = false;
	   do120 = false;
	   do130 = false;
	   do140 = false;
    } // if (do90)

    if (do100) {
       w1[0] = bup(b0,a0,y0,x0,n,eps);
       b0 = b0 + n;
    } // if (do100)

    if (do110) {
       bgrat(b0,a0,y0,x0,w1,15.0*eps,ierr1);

       w[0] = 0.5 + (0.5-w1[0]);
       do120 = false;
	   do130 = false;
	   do140 = false;
    } // if (do110)

    if (do120) {
       n = (int)b0;
       b0 = b0 - n;
       if (b0==0.0) {
         n = n - 1;
         b0 = 1.0;
       }

       w[0] = bup(b0,a0,y0,x0,n,eps);

       if (x0>0.7) {
    	   
       }

       w[0] = w[0] + bpser(a0,b0,x0,eps);
       w1[0] = 0.5 + (0.5-w[0]);
       do130 = false;
	   do140 = false;
    } // if (do120)

    if (do130) {
       if (a0<=15.0) {
         n = 20;
         w[0] = w[0] + bup(a0,b0,x0,y0,n,eps);
         a0 = a0 + n;
       }

       bgrat(a0,b0,x0,y0,w,15.0*eps,ierr1);

       w1[0] = 0.5 + (0.5-w[0]);
       do140 = false;
    } // if (do130)

    if (do140) {

       w[0] = basym(a0,b0,lambda,100.0*eps);

       w1[0] = 0.5 + (0.5-w[0]);
    } // if (do140)*/

    //  TERMINATION OF THE PROCEDURE



       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;


     }
     
     private double fpser(double a,double b, double x, double eps) {
     /*-----------------------------------------------------------------------
     !                 EVALUATION OF I (A,B)
     !                                X
     !          FOR B .LT. MIN(EPS,EPS*A) AND X .LE. 0.5.
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: fpser
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, eps, x
     ! ..
     ! .. Local Scalars ..
             REAL (dpkind) :: an, c, s, t, tol
     ! ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, LOG
     ! ..
     !                  SET  FPSER = X**A */
    	 double result = 1.0;
    	 double t, tol, an, s, c;

             if (a>1.E-3*eps) {
               result = 0.0;
               t = a*Math.log(x);
               if (t<exparg(1)) return result;
               result = Math.exp(t);
             }

             // NOTE THAT 1/B(A,B) = B

             result = (b/a)*result;
             tol = eps/a;
             an = a + 1.0;
             t = x;
             s = t/an;

             while (true) {
               an = an + 1.0;
               t = x*t;
               c = t/an;
               s = s + c;

               if (Math.abs(c)<=tol) break;
             }

             result = result*(1.0+a*s);

             return result;

     }
     
     private double exparg(int l) {
     /* .. Function Return Value ..
             REAL (dpkind) :: exparg
     ! ..
     ! .. Scalar Arguments ..
             INTEGER :: l
     ! ..
     ! .. Intrinsic Functions ..
             INTRINSIC HUGE, LOG, TINY
     ! ..
     !--------------------------------------------------------------------
     !     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
     !     EXP(W) CAN BE COMPUTED.
     !     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
     !     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
     !     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
     !--------------------------------------------------------------------*/
    	 double result;
             if (l==0) {
               result = Math.log(Double.MAX_VALUE);
             }
             else {
               result = Math.log(Double.MIN_NORMAL);
             }
             return result;

     }

     private double apser(double a,double b,double x,double eps) {
     /*-----------------------------------------------------------------------
     !     APSER YIELDS THE INCOMPLETE BETA RATIO I(SUB(1-X))(B,A) FOR
     !     A .LE. MIN(EPS,EPS*B), B*X .LE. 1, AND X .LE. 0.5. USED WHEN
     !     A IS VERY SMALL. USE ONLY IF ABOVE INEQUALITIES ARE SATISFIED.
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: apser
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, eps, x
     ! ..
     ! .. Local Scalars ..
             REAL (dpkind) :: aj, bx, c, j, s, t, tol
     ! ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, LOG
     ! ..
     ! .. Parameters ..
             REAL (dpkind), PARAMETER :: g = .577215664901533_dpkind
     ! ..*/
    	 final double g = .577215664901533;
    	 double bx, t, c, j, s, tol, aj;
    	 double result;
             bx = b*x;
             t = x - bx;

             if (b*eps<=2.E-2) {
            	 c = 0.0;
               //c = Math.log(x) + psi(b) + g + t;
             }
             else {
               c = Math.log(bx) + g + t;
             }

             tol = 5.0*eps*Math.abs(c);
             j = 1.0;
             s = 0.0;

             while (true) {
               j = j + 1.0;
               t = t*(x-bx/j);
               aj = t/j;
               s = s + aj;

               if (Math.abs(aj)<=tol) break;
             }

             result = -a*(c+s);

             return result;

     }
    
}