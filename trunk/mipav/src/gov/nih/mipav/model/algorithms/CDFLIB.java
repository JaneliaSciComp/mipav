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

    if (do70) {
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

    /*if (do100) {
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
     
     private double bup(double a,double b,double x,double y,int n,double eps) {
     /*-----------------------------------------------------------------------
     !     EVALUATION OF IX(A,B) - IX(A+N,B) WHERE N IS A POSITIVE INTEGER.
     !     EPS IS THE TOLERANCE USED.
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: bup
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, eps, x, y
             INTEGER, INTENT (IN) :: n
     ! ..
     ! .. Local Scalars ..*/
             double ap1, apb, d, l, r, t, w, result;
             int i, k, kp1, mu, nm1;
             boolean do10 = true;
             boolean do20 = true;
             boolean do30 = true;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, MIN
     ! ..
     !          OBTAIN THE SCALING FACTOR EXP(-MU) AND
     !             EXP(MU)*(X**A*Y**B/BETA(A,B))/A */
             apb = a + b;
             ap1 = a + 1.0;
             mu = 0;
             d = 1.0;

             if (n != 1 &&  a>=1.0) {
               if (apb>=1.1*ap1) {
                 mu = (int)Math.abs(exparg(1));
                 k = (int)exparg(0);
                 mu = Math.min(k,mu);
                 t = mu;
                 d = Math.exp(-t);

               }
             }

             result = brcmp1(mu,a,b,x,y)/a;

             if (n==1 || result==0.0) return result;

             nm1 = n - 1;
             w = d;

     //          LET K BE THE INDEX OF THE MAXIMUM TERM

             k = 0;
             if (b<=1.0) {
            	 do10 = false;
            	 do20 = false;
             }
             else if (y>1.E-4) {
            	 
             }
             else {
                 k = nm1;
                 do10 = false;
             }

     if (do10) {
             r = (b-1.0)*x/y - a;
             if (r<1.0) {
            	 do20 = false;
             }
             else {
	             k = nm1;
	             t = nm1;
	             if (r<t) k = (int)r;
             }
     } // if (do10)

     //          ADD THE INCREASING TERMS OF THE SERIES

     if (do20) {
             for (i = 1; i <= k; i++) {
               l = i - 1;
               d = ((apb+l)/(ap1+l))*x*d;
               w = w + d;
             }

             if (k==nm1) {
            	 do30 = false;
             }
     } // if (do20)

     //          ADD THE REMAINING TERMS OF THE SERIES

     if (do30) {
             kp1 = k + 1;

             for (i = kp1; i <= nm1; i++) {
               l = i - 1;
               d = ((apb+l)/(ap1+l))*x*d;
               w = w + d;
               if (d<=eps*w) break;
             }
     } // if (do30)

             result = result*w;

             return result;

     }
     
     private double brcmp1(int mu,double a,double b,double x,double y) {
     /*-----------------------------------------------------------------------
     !          EVALUATION OF  EXP(MU) * (X**A*Y**B/BETA(A,B))
     !-----------------------------------------------------------------------
     !-----------------
     !     CONST = 1/SQRT(2*PI)
     !-----------------
     ! .. Function Return Value ..
             REAL (dpkind) :: brcmp1
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, x, y
             INTEGER, INTENT (IN) :: mu
     ! ..
     ! .. Local Scalars ..*/
             double a0, apb, b0, c, e, h, lambda, lnx, lny, t, u, v,
               x0, y0, z, result;
             int i, n;
             boolean do10 = true;
             boolean do20 = true;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, LOG, MAX, MIN, SQRT
     ! ..
     ! .. Parameters ..*/
             final double constant = .398942280401433;
     
             /*a0 = Math.min(a,b);

             if (a0>=8.0) {
            	 do10 = false;
            	 do20 = false;
             }
             else { // a0 < 8.0

	             if (x<=0.375) {
	               lnx = Math.log(x);
	               lny = alnrel(-x);
	             }
	             else {
	
	               if (y<=0.375) {
	                 lnx = alnrel(-y);
	                 lny = Math.log(y);
	               }
	               else {
	                 lnx = Math.log(x);
	                 lny = Math.log(y);
	               }
	             }
	
	             z = a*lnx + b*lny;
	
	             if (a0>=1.0) {
	               z = z - betaln(a,b);
	               result = esum(mu,z);
	
	               return result;
	             }
	
	     //-----------------------------------------------------------------------
	     //              PROCEDURE FOR A .LT. 1 OR B .LT. 1
	     //-----------------------------------------------------------------------
	             b0 = Math.max(a,b);
	             if (b0>=8.0) {
	            	 do10 = false;
	             }
	             else if (b0>1.0) {
	            	 
	             }
	             else {
	
	     //                   ALGORITHM FOR B0 .LE. 1
	
		             result = esum(mu,z);
		
		             if (result==0.0) return result;
		
		             apb = a + b;
		             if (apb<=1.0) {
		               z = 1.0 + gam1(apb);
		             }
		             else {
		               u = a + b - 1.0;
		               z = (1.0+gam1(u))/apb;
		             }
		
		             c = (1.0+gam1(a))*(1.0+gam1(b))/z;
		             result = result*(a0*c)/(1.0+a0/b0);
		
		             return result;
	             } // else
             } // else a0 < 8.0

     //                ALGORITHM FOR 1 .LT. B0 .LT. 8

     if (do10) {
             u = gamln1(a0);
             n = (int)(b0 - 1.0);

             if (n>=1) {
               c = 1.0;
               for  (i = 1; i <= n; i++) {
                 b0 = b0 - 1.0;
                 c = c*(b0/(a0+b0));
               }
               u = Math.log(c) + u;

             }

             z = z - u;
             b0 = b0 - 1.0;
             apb = a0 + b0;

             if (apb<=1.0) {
               t = 1.0 + gam1(apb);
             }
             else {
               u = a0 + b0 - 1.0;
               t = (1.0+gam1(u))/apb;
             }

             result = a0*esum(mu,z)*(1.0+gam1(b0))/t;

             return result;
     } // if (do10)

     //                   ALGORITHM FOR B0 .GE. 8

     if (do20) {
             u = gamln1(a0) + algdiv(a0,b0);
             result = a0*esum(mu,z-u);

             return result;
     } // if (do20)
     !-----------------------------------------------------------------------
     !              PROCEDURE FOR A .GE. 8 AND B .GE. 8
     !-----------------------------------------------------------------------
     30      CONTINUE
             IF (a<=b) THEN
               h = a/b
               x0 = h/(one+h)
               y0 = one/(one+h)
               lambda = a - (a+b)*x
             ELSE
               h = b/a
               x0 = one/(one+h)
               y0 = h/(one+h)
               lambda = (a+b)*y - b
             END IF

             e = -lambda/a
             IF (ABS(e)<=0.6_dpkind) THEN
               u = rlog1(e)
             ELSE
               u = e - LOG(x/x0)
             END IF

             e = lambda/b

             IF (ABS(e)<=0.6_dpkind) THEN
               v = rlog1(e)
             ELSE
               v = e - LOG(y/y0)
             END IF

             z = esum(mu,-(a*u+b*v))

             brcmp1 = const*SQRT(b*x0)*z*EXP(-bcorr(a,b))*/
             return 0.0;

    }

     
     private double bfrac(double a,double b,double x,double y,double lambda,double eps) {
     /*-----------------------------------------------------------------------
     !     CONTINUED FRACTION EXPANSION FOR IX(A,B) WHEN A,B .GT. 1.
     !     IT IS ASSUMED THAT  LAMBDA = (A + B)*Y - B.
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: bfrac
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, eps, lambda, x, y
     ! ..
     ! .. Local Scalars ..*/
             double alpha, an, anp1, beta, bn, bnp1, c, c0, c1, e,
               n, p, r, r0, s, t, w, yp1, result;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS
     */
             result = brcomp(a,b,x,y);

             if (result==0.0) return result;

             c = 1.0 + lambda;
             c0 = b/a;
             c1 = 1.0 + 1.0/a;
             yp1 = y + 1.0;

             n = 0.0;
             p = 1.0;
             s = a + 1.0;
             an = 0.0;
             bn = 1.0;
             anp1 = 1.0;
             bnp1 = c/c1;
             r = c1/c;

     //        CONTINUED FRACTION CALCULATION

     while (true) {
             n = n + 1.0;
             t = n/a;
             w = n*(b-n)*x;
             e = a/s;
             alpha = (p*(p+c0)*e*e)*(w*x);
             e = (1.0+t)/(c1+t+t);
             beta = n + w/s + e*(c+n*yp1);
             p = 1.0 + t;
             s = s + 2.0;

     //        UPDATE AN, BN, ANP1, AND BNP1

             t = alpha*an + beta*anp1;
             an = anp1;
             anp1 = t;
             t = alpha*bn + beta*bnp1;
             bn = bnp1;
             bnp1 = t;

             r0 = r;
             r = anp1/bnp1;
             if (Math.abs(r-r0)<=eps*r) break;

     //        RESCALE AN, BN, ANP1, AND BNP1

             an = an/bnp1;
             bn = bn/bnp1;
             anp1 = r;
             bnp1 = 1.0;
     } // while (true)

             result = result*r;

             return result;

     }
     
     private double brcomp(double a,double b,double x,double y) {
     /*-----------------------------------------------------------------------
     !               EVALUATION OF X**A*Y**B/BETA(A,B)
     !-----------------------------------------------------------------------
     !-----------------
     !     CONST = 1/SQRT(2*PI)
     !-----------------
     ! .. Function Return Value ..
             REAL (dpkind) :: brcomp
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, x, y
     ! ..
     ! .. Local Scalars ..*/
             double a0, apb, c, e, h, lambda, lnx, lny, t, u, v,
               x0, y0, result;
             double b0 = 0.0;
             double z = 0.0;
             int i, n;
             boolean do10 = true;
             boolean do20 = true;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, LOG, MAX, MIN, SQRT
     ! ..
     ! .. Parameters ..*/
             final double constant = .398942280401433;
     
             result = 0.0;

             if (x<=0.0 || y<=0.0) return result;

             a0 = Math.min(a,b);

             if (a0>=8.0) {
            	 do10 = false;
            	 do20 = false;
             }
             else { // a0 < 8.0

	             if (x<=0.375) {
	               lnx = Math.log(x);
	               lny = alnrel(-x);
	             }
	             else {
	               if (y<=0.375) {
	                 lnx = alnrel(-y);
	                 lny = Math.log(y);
	               }
	               else {
	                 lnx = Math.log(x);
	                 lny = Math.log(y);
	               }
	             }
	
	             z = a*lnx + b*lny;
	
	             if (a0>=1.0) {
	               z = z - betaln(a,b);
	               result = Math.exp(z);
	
	               return result;
	             }
	
	     //-----------------------------------------------------------------------
	     //              PROCEDURE FOR A .LT. 1 OR B .LT. 1
	     //-----------------------------------------------------------------------
	
	             b0 = Math.max(a,b);
	
	             if (b0>=8.0) {
	            	 do10 = false;
	             }
	             else if (b0>1.0) {
	            	 
	             }
	
	     //                   ALGORITHM FOR B0 .LE. 1
	             else {
		             result = Math.exp(z);
		
		             if (result==0.0) return result;
		
		             apb = a + b;
		
		             if (apb<=1.0) {
		               z = 1.0 + gam1(apb);
		             }
		             else {
		               u = a + b - 1.0;
		               z = (1.0+gam1(u))/apb;
		             }
		
		             c = (1.0+gam1(a))*(1.0+gam1(b))/z;
		             result = result*(a0*c)/(1.0+a0/b0);
		             return result;
	             } // else
             } // else a0 < 8.0

     //                ALGORITHM FOR 1 .LT. B0 .LT. 8

     if (do10) {
             u = gamln1(a0);
             n = (int)(b0 - 1.0);

             if (n>=1) {
               c = 1.0;
               for (i = 1; i <= n; i++) {
                 b0 = b0 - 1.0;
                 c = c*(b0/(a0+b0));
               }

               u = Math.log(c) + u;

             } // if (n >= 1)

             z = z - u;
             b0 = b0 - 1.0;
             apb = a0 + b0;

             if (apb<=1.0) {
               t = 1.0 + gam1(apb);
             }
             else {
               u = a0 + b0 - 1.0;
               t = (1.0+gam1(u))/apb;
             }

             result = a0*Math.exp(z)*(1.0+gam1(b0))/t;
             return result;
     } // if (do10)

     //                   ALGORITHM FOR B0 .GE. 8

     if (do20) {
             u = gamln1(a0) + algdiv(a0,b0);
             result = a0*Math.exp(z-u);
             return result;
     } // if (do20)
     //-----------------------------------------------------------------------
     //              PROCEDURE FOR A .GE. 8 AND B .GE. 8
     //-----------------------------------------------------------------------
    
             if (a<=b) {
               h = a/b;
               x0 = h/(1.0+h);
               y0 = 1.0/(1.0+h);
               lambda = a - (a+b)*x;
             }
             else {
               h = b/a;
               x0 = 1.0/(1.0+h);
               y0 = h/(1.0+h);
               lambda = (a+b)*y - b;
             }

             e = -lambda/a;

             if (Math.abs(e)<=0.6) {
               u = rlog1(e);
             }
             else {
               u = e - Math.log(x/x0);
             }

             e = lambda/b;

             if (Math.abs(e)<=0.6) {
               v = rlog1(e);
             }
             else {
               v = e - Math.log(y/y0);
             }
             z = Math.exp(-(a*u+b*v));

             result = constant*Math.sqrt(b*x0)*z*Math.exp(-bcorr(a,b));
             return result;

     }
     
     private double rlog1(double x) {
     /*-----------------------------------------------------------------------
     !             EVALUATION OF THE FUNCTION X - LN(1 + X)
     !-----------------------------------------------------------------------
     !------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: rlog1
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: x
     ! ..
     ! .. Local Scalars ..*/
             double h, r, t, w, w1, result;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC LOG
     ! ..
     ! .. Parameters ..*/
             final double a = .566749439387324E-01;
             final double b = .456512608815524E-01;
             final double p[] = new double[]{.333333333333333,
                -.224696413112536, .620886815375787E-02};
             final double q[] = new double[] {1.0,
               -.127408923933623E+01, .354508718369557};
     
             if (x<=-1.0) {
               result = -1.0;
             }
             else if (x>=(-0.39) && x<=0.57) {
               if (x<-0.18) {
                 h = x + 0.3;
                 h = h/0.7;
                 w1 = a - h*0.3;
               }
               else if (x>0.18) {
                 h = 0.75*x - 0.25;
                 w1 = b + h/3.0;
               }
               else {
     // ARGUMENT REDUCTION

                 h = x;
                 w1 = 0.0;
               }

     // SERIES EXPANSION

               r = h/(h+2.0);
               t = r*r;

               w = evaluate_polynomial(p,t)/evaluate_polynomial(q,t);

               result = 2.0*t*(1.0/(1.0-r)-r*w) + w1;
             }
             else {
               w = (x+0.5) + 0.5;
               result = x - Math.log(w);
             }
             return result;
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
                 c = Math.log(x) + psi(b) + g + t;
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
     
   

    private double psi(double xx) {
/*---------------------------------------------------------------------
!                 EVALUATION OF THE DIGAMMA FUNCTION
!                           -----------
!     PSI(XX) IS ASSIGNED THE VALUE 0 WHEN THE DIGAMMA FUNCTION CANNOT
!     BE COMPUTED.
!     THE MAIN COMPUTATION INVOLVES EVALUATION OF RATIONAL CHEBYSHEV
!     APPROXIMATIONS PUBLISHED IN MATH. COMP. 27, 123-127(1973) BY
!     CODY, STRECOK AND THACHER.
!---------------------------------------------------------------------
!     PSI WAS WRITTEN AT ARGONNE NATIONAL LABORATORY FOR THE FUNPACK
!     PACKAGE OF SPECIAL FUNCTION SUBROUTINES. PSI WAS MODIFIED BY
!     A.H. MORRIS (NSWC).
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!     PIOV4 = PI/4
!     DX0 = ZERO OF PSI TO EXTENDED PRECISION
!---------------------------------------------------------------------
!     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
!     PSI(X) / (X - X0),  0.5 .LE. X .LE. 3.0
!---------------------------------------------------------------------
!     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
!     PSI(X) - LN(X) + 1 / (2*X),  X .GT. 3.0
!---------------------------------------------------------------------
! .. Function Return Value ..
       REAL (dpkind) :: psi
! ..
! .. Scalar Arguments ..
       REAL (dpkind) :: xx
! ..
! .. Local Scalars ..
       REAL (dpkind) :: aug, den, sgn, w, x, xmax1, xmx0, xsmall, z
       INTEGER :: m, n, nq
! ..
! .. Intrinsic Functions ..
       INTRINSIC ABS, COS, EPSILON, HUGE, INT, LOG, MIN, REAL, SIN
! ..
! .. Parameters ..*/
       double dx0 = 1.461632144968362341262659542325721325;
       double piov4 = .785398163397448;
       double p1[] = new double[]{
         .130560269827897E+04, .413810161269013E+04,
         .363351846806499E+04, .118645200713425E+04,
         .142441585084029E+03, .477762828042627E+01,
         .895385022981970E-02};
       double p2[] = new double[]{0.0,
         -.648157123766197E+00, -.448616543918019E+01,
         -.701677227766759E+01, -.212940445131011E+01};
       double q1[] = new double[] {
         .691091682714533E-05, .190831076596300E+04,
         .364127349079381E+04, .221000799247830E+04,
         .520752771467162E+03, .448452573429826E+02, 1.0};
       double q2[] = new double[] {
         .777788548522962E+01, .546117738103215E+02,
         .892920700481861E+02, .322703493791143E+02, 1.0};
/* ..
!---------------------------------------------------------------------
!     MACHINE DEPENDENT CONSTANTS ...
!        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT
!                 WITH ENTIRELY INTEGER REPRESENTATION.  ALSO USED
!                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE
!                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH
!                 PSI MAY BE REPRESENTED AS ALOG(X).
!        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X)
!                 MAY BE REPRESENTED BY 1/X.
!---------------------------------------------------------------------*/
   double xmax1;
   double xsmall;
   double x;
   double aug;
   double w;
   double sgn = 0.0;
   double z = 0.0;
   double den;
   double xmx0;
   int nq;
   int n;
   int m;
   boolean do10 = true;
   boolean do20 = true;
   boolean do30 = true;
       xmax1 = Integer.MAX_VALUE;
       xmax1 = Math.min(xmax1,1.0/EPSILON());
       xsmall = 1.E-9;
//---------------------------------------------------------------------
       x = xx;
       aug = 0.0;

       if (x>= 0.5) {
    	    do10 = false;
    	    do20 = false;
    	    do30 = false;
       }

//---------------------------------------------------------------------
//     X .LT. 0.5,  USE REFLECTION FORMULA
//     PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
//---------------------------------------------------------------------
       else if (Math.abs(x)>xsmall) {
    	   
       }
       else if (x==0.0) {
    	   // Error return
    	   return 0.0;
       }

//---------------------------------------------------------------------
//     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE
//     FOR  PI*COTAN(PI*X)
//---------------------------------------------------------------------
       else {
           aug = -1.0/x;
           do10 = false;
           do20 = false;
       }

//---------------------------------------------------------------------
//     REDUCTION OF ARGUMENT FOR COTAN
//---------------------------------------------------------------------
    if (do10) {
       w = -x;
       sgn = piov4;
       if (w<=0.0) {
         w = -w;
         sgn = -sgn;
//---------------------------------------------------------------------
//     MAKE AN ERROR EXIT IF X .LE. -XMAX1
//---------------------------------------------------------------------
       }

       if (w>=xmax1) {
    	   // Error return 
    	   return 0.0;
       }
       else { // w < xmax1
            nq = (int)w;
            w = w - (double)nq;
            nq = (int)(w*4.0);
            w = 4.0*(w-((double)nq)*.25);

//---------------------------------------------------------------------
//     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X.
//     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST
//     QUADRANT AND DETERMINE SIGN
//---------------------------------------------------------------------
	       n = nq/2;
	       if (n+n!=nq) w = 1.0 - w;
	
	       z = piov4*w;
	       m = n/2;
	
	       if (m+m!=n) sgn = -sgn;

//---------------------------------------------------------------------
//     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X)
//---------------------------------------------------------------------
	       n = (nq+1)/2;
	       m = n/2;
	       m = m + m;
	       if (m == n) {

//---------------------------------------------------------------------
//     CHECK FOR SINGULARITY
//---------------------------------------------------------------------
               if (z==0.0) {
            	   // Error return 
            	   return 0.0;
               }
               else { // z != 0.0
//---------------------------------------------------------------------
//     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND
//     SIN/COS AS A SUBSTITUTE FOR TAN
//---------------------------------------------------------------------
                   aug = sgn*((Math.cos(z)/Math.sin(z))*4.0);
                   do20 = false;
               } // else z != 0.0
	       } // if (m == n)
       } // else w < xmax1
    } // if (do10)

    if (do20) {
       aug = sgn*((Math.sin(z)/Math.cos(z))*4.0);
    } // if (do20)

    if (do30) {
       x = 1.0 - x;
    } // if (do30)

       if (x<=3.0) {
//---------------------------------------------------------------------
//     0.5 .LE. X .LE. 3.0
//---------------------------------------------------------------------

         den = evaluate_polynomial(p1,x)/evaluate_polynomial(q1,x);

         xmx0 = x - dx0;
         return den*xmx0 + aug;

//---------------------------------------------------------------------
//     IF X .GE. XMAX1, PSI = LN(X)
//---------------------------------------------------------------------
       } // if (x <= 3.0)

       if (x<xmax1) {
//---------------------------------------------------------------------
//     3.0 .LT. X .LT. XMAX1
//---------------------------------------------------------------------
         w = 1.0/(x*x);

         aug = evaluate_polynomial(p2,w)/evaluate_polynomial(q2,w) - 
           0.5/x + aug;
       } // if (x < xmax1)

       return aug + Math.log(x);



    }
    
    

    private double evaluate_polynomial(double a[], double x) {
/*----------------------------------------------------------------------
!              Evaluate a PoLynomial at x
!                              Function
!     Returns:
!          A(1) + A(2)*X + ... + A(N)*X**(N-1)
!                              Arguments
!     A --> Array of coefficients of the polynomial.
!     X --> Point at which the polynomial is to be evaluated.
!----------------------------------------------------------------------
! .. Function Return Value ..
      REAL (dpkind) :: evaluate_polynomial
! ..
! .. Scalar Arguments ..
      REAL (dpkind), INTENT (IN) :: x
! ..
! .. Array Arguments ..
      REAL (dpkind), INTENT (IN) :: a(:)
! ..
! .. Local Scalars ..
      REAL (dpkind) :: term
      INTEGER :: i, n
! ..
! .. Intrinsic Functions ..
      INTRINSIC SIZE
! ..*/
   int n;
   int i;
   double term;
      n = a.length;
      term = a[n-1];

      for (i = n - 2; i >= 0; i--) {
        term = a[i] + term*x;
      }

      return term;

  }

     
    private double bpser(double a,double b, double x, double eps) {
    /*-----------------------------------------------------------------------
    !     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B .LE. 1
    !     OR B*X .LE. 0.7.  EPS IS THE TOLERANCE USED.
    !-----------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: bpser
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind), INTENT (IN) :: a, b, eps, x
    ! ..
    ! .. Local Scalars ..*/
            double a0, apb, c, n, sum, t, tol, u, w, z;
            double b0 = 0.0;
            int i, m;
    /*
    ! .. Intrinsic Functions ..
            INTRINSIC ABS, EXP, LOG, MAX, MIN, REAL
    ! ..*/
    	    
            double result = 0.0;
            boolean do10 = true;
            boolean do20 = true;
            boolean do30 = true;
            if (x==0.0) return result;
    //-----------------------------------------------------------------------
    //            COMPUTE THE FACTOR X**A/(A*BETA(A,B))
    //-----------------------------------------------------------------------
            a0 = Math.min(a,b);
            if (a0 >= 1.0) {
                    z = a*Math.log(x) - betaln(a,b);
                    result = Math.exp(z)/a;
                    do10 = false;
                    do20 = false;
                    do30 = false;
            } // if (a0 >= 1.0)

    if (do10) {
            b0 = Math.max(a,b);
            if (b0>=8.0) {
            	do20 = false;
            }
            else if (b0>1.0) {
            	
            }
            else {
    //            PROCEDURE FOR A0 .LT. 1 AND B0 .LE. 1

	            result = Math.pow(x,a);
	            if (result==0.0) return result;
	
	            apb = a + b;
	            if (apb<=1.0) {
	                z = 1.0 + gam1(apb);
	            }
	            else {
	              u = a + b - 1.0;
	              z = (1.0+gam1(u))/apb;
	            }
	
	            c = (1.0+gam1(a))*(1.0+gam1(b))/z;
	            result = result*c*(b/apb);
	            do20 = false;
	            do30 = false;
            } // else
    } // if (do10)

    //         PROCEDURE FOR A0 .LT. 1 AND 1 .LT. B0 .LT. 8

    if (do20) {
            u = gamln1(a0);
            m = (int)(b0 - 1.0);

            if (m>=1) {
              c = 1.0;
              for (i = 1; i <= m; i++) {
                b0 = b0 - 1.0;
                c = c*(b0/(a0+b0));
              }

              u = Math.log(c) + u;
            } // if (m >= 1)

            z = a*Math.log(x) - u;
            b0 = b0 - 1.0;
            apb = a0 + b0;

            if (apb<=1.0) {
              t = 1.0 + gam1(apb);
            }
            else {
              u = a0 + b0 - 1.0;
              t = (1.0+gam1(u))/apb;
            }

            result = Math.exp(z)*(a0/a)*(1.0+gam1(b0))/t;
            do30 = false;
    } // if (do20)

    //            PROCEDURE FOR A0 .LT. 1 AND B0 .GE. 8

    if (do30) {
            u = gamln1(a0) + algdiv(a0,b0);
            z = a*Math.log(x) - u;
            result = (a0/a)*Math.exp(z);
    } // if (do30)

        if (result==0.0 || a<=0.1*eps) return result;
//-----------------------------------------------------------------------
//                     COMPUTE THE SERIES
//-----------------------------------------------------------------------
        sum = 0.0;
        n = 0;
        c = 1.0;
        tol = eps/a;

        do {
	        n = n + 1;
	        c = c*(0.5+(0.5-b/n))*x;
	        w = c/(a+n);
	        sum = sum + w;
        } while (Math.abs(w)>tol);

        result = result*(1.0+a*sum);

        return result;

    }
    
    private double gam1(double a) {
    /*     ------------------------------------------------------------------
    !     COMPUTATION OF 1/GAMMA(A+1) - 1  FOR -0.5 .LE. A .LE. 1.5
    !     ------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: gam1
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind), INTENT (IN) :: a
    ! ..
    ! .. Local Scalars ..*/
            double d, t, w, result;
    
    // Parameters ..
            final double p[] = new double[]{.577215664901533,
              -.409078193005776, -.230975380857675,
              .597275330452234E-01, .766968181649490E-02,
              -.514889771323592E-02, .589597428611429E-03};
            final double q[] = new double[]{ .100000000000000E+01,
               .427569613095214, .158451672430138,
              .261132021441447E-01, .423244297896961E-02};
            final double r[] =  new double[]{
              -.422784335098468, -.771330383816272,
              -.244757765222226, .118378989872749,
              .930357293360349E-03, -.118290993445146E-01,
              .223047661158249E-02, .266505979058923E-03,
              -.132674909766242E-03};
            final double s1 = .273076135303957;
            final double s2 = .559398236957378E-01;
    
            t = a;
            d = a - 0.5;

            if (d>0.0) t = d - 0.5;

            if (t==0.0) {
              result = 0.0;
            }
            else if (t>0.0) {
              w = evaluate_polynomial(p,t)/evaluate_polynomial(q,t);

              if (d<=0.0) {
                result = a*w;
              }
              else {
                result = (t/a)*((w-0.5)-0.5);
              }
            }
            else {
              w = evaluate_polynomial(r,t)/((s2*t+s1)*t+1.0);

              if (d<=0.0) {
                result = a*((w+0.5)+0.5);
              }
              else {
                result = t*w/a;
              }
            }
            return result;
    }
    
    private double betaln(double a0,double b0) {
    /*-----------------------------------------------------------------------
    !     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION
    !-----------------------------------------------------------------------
    !     E = 0.5*LN(2*PI)
    !--------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: betaln
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind) :: a0, b0
    ! ..
    ! .. Local Scalars ..
            REAL (dpkind) :: a, b, c, h, u, v, w, z
            INTEGER :: i, n
    ! ..
    ! .. Intrinsic Functions ..
            INTRINSIC LOG, MAX, MIN
    ! ..
    ! .. Parameters ..*/
            final double e = .918938533204673;
    double a;
    double b;
    double h;
    double w = 0.0;
    double z;
    double c;
    double u;
    double v;
    double result;
    int i;
    int n;
    boolean do10 = true;
    boolean do20 = true;
    boolean do30 = true;
    boolean do40 = true;
            a = Math.min(a0,b0);
            b = Math.max(a0,b0);

            if (a>= 8.0) {
            	do10 = false;
            	do20 = false;
            	do30 = false;
            	do40 = false;
            }
            else if (a>=1.0) {
            
            }

    //-----------------------------------------------------------------------
    //                   PROCEDURE WHEN A .LT. 1
    //-----------------------------------------------------------------------
            else if (b<8.0) {
                return gamln(a) + (gamln(b)-gamln(a+b));
            }

            else {

                return gamln(a) + algdiv(a,b);

            }

    //-----------------------------------------------------------------------
    //                PROCEDURE WHEN 1 .LE. A .LT. 8
    //-----------------------------------------------------------------------
    if (do10) {
            if (a <= 2.0) {
	            if (b<=2.0) {
	              result = gamln(a) + gamln(b) - gsumln(a,b);
	              return result;
	
	            }
	
	            w = 0.0;
	            if (b<8.0) {
	            	do20 = false;
	            }
	            else {
	                result = gamln(a) + algdiv(a,b);
	                return result;
	            }
            } // if (a <= 2.0)
    } // if (do10)

    //                REDUCTION OF A WHEN B .LE. 1000

    if (do20) {
            if (b>1000.0) {
            	do30 = false;
            }
            else { // b <= 1000
	            n = (int)(a - 1.0);
	            w = 1.0;
	
	            for (i = 1; i <=  n; i++) {
	              a = a - 1.0;
	              h = a/b;
	              w = w*(h/(1.0+h));
	            }
	
	            w = Math.log(w);
	
	            if (b>=8.0) {
	              result = w + gamln(a) + algdiv(a,b);
	              return result;
	
	    //                 REDUCTION OF B WHEN B .LT. 8
	
	            }
            } // else b <= 1000
    } // if (do20)

    if (do30) {
            n = (int)(b - 1.0);
            z = 1.0;

            for  (i = 1; i <= n; i++) {
              b = b - 1.0;
              z = z*(b/(a+b));
            }

            result = w + Math.log(z) + (gamln(a)+(gamln(b)-gsumln(a,b)));
            return result;
    } // if (d030)

    //                REDUCTION OF A WHEN B .GT. 1000

    if (do40) {
            n = (int)(a - 1.0);
            w = 1.0;

            for (i = 1; i <= n; i++) {
              a = a - 1.0;
              w = w*(a/(1.0+a/b));
            }

            result = (Math.log(w)-n*Math.log(b)) + (gamln(a)+algdiv(a,b));
            return result;
    } // if (do40)

    //-----------------------------------------------------------------------
    //                   PROCEDURE WHEN A .GE. 8
    //-----------------------------------------------------------------------
            w = bcorr(a,b);
            h = a/b;
            c = h/(1.0+h);
            u = -(a-0.5)*Math.log(c);
            v = b*alnrel(h);

            if (u>v) {
              result = ((((-0.5*Math.log(b))+e)+w)-v) - u;
              return result;

            }

            result = ((((-0.5*Math.log(b))+e)+w)-u) - v;
            return result;

    }
    
    private double bcorr(double a0, double b0) {
    /*-----------------------------------------------------------------------
    !     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE
    !     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A).
    !     IT IS ASSUMED THAT A0 .GE. 8 AND B0 .GE. 8.
    !-----------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: bcorr
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind) :: a0, b0
    ! ..
    ! .. Local Scalars ..*/
            double a, b, c, h, s11, s3, s5, s7, s9, t, w, x, x2, result;
    /*
    ! .. Intrinsic Functions ..
            INTRINSIC MAX, MIN
    ! ..
    ! .. Parameters ..*/
            final double c0 = .833333333333333E-01;
            final double c1 = -.277777777760991E-02;
            final double c2 = .793650666825390E-03;
            final double c3 = -.595202931351870E-03;
            final double c4 = .837308034031215E-03;
            final double c5 = -.165322962780713E-02;
   
            a = Math.min(a0,b0);
            b = Math.max(a0,b0);

            h = a/b;
            c = h/(1.0+h);
            x = 1.0/(1.0+h);
            x2 = x*x;

    //                SET SN = (1 - X**N)/(1 - X)

            s3 = 1.0 + (x+x2);
            s5 = 1.0 + (x+x2*s3);
            s7 = 1.0 + (x+x2*s5);
            s9 = 1.0 + (x+x2*s7);
            s11 = 1.0 + (x+x2*s9);

    //                SET W = DEL(B) - DEL(A + B)

            t = (1.0/b);
            t = t *t;
            w = ((((c5*s11*t+c4*s9)*t+c3*s7)*t+c2*s5)*t+c1*s3)*t + c0;
            w = w*(c/b);

    //                   COMPUTE  DEL(A) + W

            t = (1.0/a);
            t = t * t;

            result = (((((c5*t+c4)*t+c3)*t+c2)*t+c1)*t+c0)/a + w;
            return result;

    }
    
    private double gsumln(double a,double b) {
    /*-----------------------------------------------------------------------
    !          EVALUATION OF THE FUNCTION LN(GAMMA(A + B))
    !          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2
    !-----------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: gsumln
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind) :: a, b
    ! ..
    ! .. Local Scalars ..
            REAL (dpkind) :: x
    ! ..
    ! .. Intrinsic Functions ..
            INTRINSIC LOG, REAL
    ! ..*/
    	double x, result;
            x = a + b - 2.0;

            if (x<=0.25) {
              return gamln1(1.0+x);
            }

            if (x<=1.25) {
              result = gamln1(x) + alnrel(x);
              return result;

            }

            result = gamln1(x-1.0) + Math.log(x*(1.0+x));
            return result;

    }

    
    private double algdiv(double a, double b) {
    /*-----------------------------------------------------------------------
    !     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B .GE. 8
    !                         --------
    !     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY
    !     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X).
    !-----------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: algdiv
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind), INTENT (IN) :: a, b
    ! ..
    ! .. Local Scalars ..
            REAL (dpkind) :: c, d, h, s11, s3, s5, s7, s9, t, u, v, w, x, x2
    ! ..
    ! .. Intrinsic Functions ..
            INTRINSIC LOG
    ! ..
    ! .. Parameters ..*/
            double c0 = .833333333333333E-01;
            double c1 = -.277777777760991E-02;
            double c2 = .793650666825390E-03;
            double c3 = -.595202931351870E-03;
            double c4 = .837308034031215E-03;
            double c5 = -.165322962780713E-02;
           double c, d, h, s11, s3, s5, s7, s9, t, u, v, w, x, x2, result;
            if (a>b) {
              h = b/a;
              c = 1.0/(1.0+h);
              x = h/(1.0+h);
              d = a + (b-0.5);
            }
            else {
              h = a/b;
              c = h/(1.0+h);
              x = 1.0/(1.0+h);
              d = b + (a-0.5);
            }

    // SET SN = (1 - X**N)/(1 - X)

            x2 = x*x;
            s3 = 1.0 + (x+x2);
            s5 = 1.0 + (x+x2*s3);
            s7 = 1.0 + (x+x2*s5);
            s9 = 1.0 + (x+x2*s7);
            s11 = 1.0 + (x+x2*s9);

    // SET W = DEL(B) - DEL(A + B)

            t = (1.0/b);
            t = t * t;
            w = ((((c5*s11*t+c4*s9)*t+c3*s7)*t+c2*s5)*t+c1*s3)*t + c0;
            w = w*(c/b);

    // COMBINE THE RESULTS

            u = d*alnrel(a/b);
            v = a*(Math.log(b)-1.0);

            if (u>v) {
              result = (w-v) - u;
            }
            else {
              result = (w-u) - v;
            }
            return result;

    }
    
    private double alnrel(double a) {
    /*-----------------------------------------------------------------------
    !            EVALUATION OF THE FUNCTION LN(1 + A)
    !-----------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: alnrel
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind), INTENT (IN) :: a
    ! ..
    ! .. Local Scalars ..
            REAL (dpkind) :: t, t2, w
    ! ..
    ! .. Intrinsic Functions ..
            INTRINSIC ABS, LOG
    ! ..
    ! .. Parameters ..*/
            double p[] = new double[]{ 1.0,
              -.129418923021993E+01, .405303492862024,
              -.178874546012214E-01};
            double q[] = new double[] {1.0,
              -.162752256355323E+01, .747811014037616,
              -.845104217945565E-01};
            double t, t2, w, result;
            if (Math.abs(a)<=0.375) {
              t = a/(a+2.0);
              t2 = t*t;

              w = evaluate_polynomial(p,t2)/evaluate_polynomial(q,t2);

              result = 2.0*t*w;
            }
            else if (a<-1.0) {
              result = -1.0;
            }
            else {
              result = Math.log(a+1.0);
            }
            return result;

    }

    
    private double gamln(double a) {
    /*-----------------------------------------------------------------------
    !            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A
    !-----------------------------------------------------------------------
    !     WRITTEN BY ALFRED H. MORRIS
    !          NAVAL SURFACE WARFARE CENTER
    !          DAHLGREN, VIRGINIA
    !--------------------------
    !     D = 0.5*(LN(2*PI) - 1)
    !--------------------------
    !--------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: gamln
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind), INTENT (IN) :: a
    ! ..
    ! .. Local Scalars ..
            REAL (dpkind) :: t, w
            INTEGER :: i, n
    ! ..
    ! .. Intrinsic Functions ..
            INTRINSIC LOG
    ! ..
    ! .. Parameters ..*/
            double c[] = new double[] { .833333333333333E-01,
              -.277777777760991E-02, .793650666825390E-03, 
              -.595202931351870E-03, .837308034031215E-03,
              -.165322962780713E-02};
            double d = .418938533204673;
            double t;
            double w;
            int i;
            int n;
            double result;
            if (a<=0.0) {
              result = -1.0;
            }
            else if (a<=0.8) {
              result = gamln1(a) - Math.log(a);
            }
            else if (a<=2.25) {
              t = (a-0.5) - 0.5;
              result = gamln1(t);
            }
            else if (a<10.0) {
              n = (int)(a - 1.25);
              t = a;
              w = 1.0;

              for (i = 1; i <= n; i++) {
                t = t - 1.0;
                w = t*w;
              }

              result = gamln1(t-1.0) + Math.log(w);
            }
            else {

              t = (1.0/a);
              t = t*t;

              w = evaluate_polynomial(c,t)/a;

              result = (d+w) + (a-0.5)*(Math.log(a)-1.0);
            }
            return result;

    }
    
    private double gamln1(double a) {
    /*-----------------------------------------------------------------------
    !     EVALUATION OF LN(GAMMA(1 + A)) FOR -0.2 .LE. A .LE. 1.25
    !-----------------------------------------------------------------------
    ! .. Function Return Value ..
            REAL (dpkind) :: gamln1
    ! ..
    ! .. Scalar Arguments ..
            REAL (dpkind), INTENT (IN) :: a
    ! ..
    ! .. Local Scalars ..
            REAL (dpkind) :: w, x
    ! ..
    ! .. Parameters ..*/
            double p[] = new double[]{ .577215664901533,
              .844203922187225, -.168860593646662,
              -.780427615533591, -.402055799310489,
              -.673562214325671E-01, -.271935708322958E-02};
            double q[] = new double[] {1.0,
              .288743195473681E+01, .312755088914843E+01,
              .156875193295039E+01, .361951990101499,
              .325038868253937E-01, .667465618796164E-03};
            double r[] = new double[]{ .422784335098467,
              .848044614534529, .565221050691933,
              .156513060486551, .170502484022650E-01,
              .497958207639485E-03};
            double s[] = new double[]{ 1.0,
              .124313399877507E+01, .548042109832463, 
              .101552187439830, .713309612391000E-02,
              .116165475989616E-03};
            double w;
            double x;
            double result;
    
            if (a<0.6) {
              w = evaluate_polynomial(p,a)/evaluate_polynomial(q,a);

              result = -a*w;
            }

            else {

              x = (a-0.5) -0.5;

              w = evaluate_polynomial(r,x)/evaluate_polynomial(s,x);

              result = x*w;
            }
            return result;

    }

     
     private int ipmpar(int i) {
    /* C-----------------------------------------------------------------------
     C
     C     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
     C     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
     C     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...
     C
     C  INTEGERS.
     C
     C     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM
     C
     C               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )
     C
     C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.
     C
     C     IPMPAR(1) = A, THE BASE.
     C
     C     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS.
     C
     C     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE.
     C
     C  FLOATING-POINT NUMBERS.
     C
     C     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
     C     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
     C     NONZERO NUMBERS ARE REPRESENTED IN THE FORM
     C
     C               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)
     C
     C               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
     C               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.
     C
     C     IPMPAR(4) = B, THE BASE.
     C
     C  SINGLE-PRECISION
     C
     C     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.
     C
     C     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.
     C
     C     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.
     C
     C  DOUBLE-PRECISION
     C
     C     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.
     C
     C     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.
     C
     C     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.
     C
     C-----------------------------------------------------------------------
     C
     C     TO DEFINE THIS FUNCTION FOR THE COMPUTER BEING USED, ACTIVATE
     C     THE DATA STATMENTS FOR THE COMPUTER BY REMOVING THE C FROM
     C     COLUMN 1. (ALL THE OTHER DATA STATEMENTS SHOULD HAVE C IN
     C     COLUMN 1.)
     C
     C-----------------------------------------------------------------------
     C
     C     IPMPAR IS AN ADAPTATION OF THE FUNCTION I1MACH, WRITTEN BY
     C     P.A. FOX, A.D. HALL, AND N.L. SCHRYER (BELL LABORATORIES).
     C     IPMPAR WAS FORMED BY A.H. MORRIS (NSWC). THE CONSTANTS ARE
     C     FROM BELL LABORATORIES, NSWC, AND OTHER SOURCES.
     C
     C-----------------------------------------------------------------------
    
     //    MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
     //     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
     //     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
     */
    	  int IMACH[] = new int[11];
           IMACH[1] =    2 ;
           IMACH[2] =    31;
           IMACH[3] = 2147483647;
           IMACH[4] =     2;
           IMACH[5] =    24;
           IMACH[6] =  -125;
           IMACH[7] =   128;
           IMACH[8] =    53;
           IMACH[9] =  -1021;
           IMACH[10] =  1024;
    
           return IMACH[i];

     }
     
     private double spmpar(int i) {
     /*-----------------------------------------------------------------------
     C
     C     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
     C     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
     C     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
     C     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
     C     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
     C
     C        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
     C
     C        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
     C
     C        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
     C
     C-----------------------------------------------------------------------
     C     WRITTEN BY
     C        ALFRED H. MORRIS, JR.
     C        NAVAL SURFACE WARFARE CENTER
     C        DAHLGREN VIRGINIA
     C-----------------------------------------------------------------------
     C-----------------------------------------------------------------------
     C     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
     C     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
     C     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
     C-----------------------------------------------------------------------
     C     .. Scalar Arguments ..
           INTEGER i
     C     ..
     C     .. Local Scalars ..
           DOUBLE PRECISION b,binv,bm1,one,w,z
           INTEGER emax,emin,ibeta,m
     C     ..
     C     .. External Functions ..
           INTEGER ipmpar
           EXTERNAL ipmpar
     C     ..
     C     .. Intrinsic Functions ..
           INTRINSIC dble
     C     ..
     C     .. Executable Statements ..
     C*/
    	 double b;
    	 double binv;
    	 double w;
    	 double one;
    	 double bm1;
    	 double z;
    	 int m;
    	 int emin;
    	 int ibeta;
    	 int emax;
           if (i <= 1) {
               b = ipmpar(4);
               m = ipmpar(8);
               return Math.pow(b,(1-m));
           }
     
           if (i <= 2) {
               b = ipmpar(4);
               emin = ipmpar(9);
               one = 1.0;
               binv = one/b;
               w = Math.pow(b,(emin+2));
               return ((w*binv)*binv)*binv;
           }
     
           ibeta = ipmpar(4);
           m = ipmpar(8);
           emax = ipmpar(10);
     
           b = ibeta;
           bm1 = ibeta - 1;
           one = 1.0;
           z = Math.pow(b,(m-1));
           w = ((z-one)*b+bm1)/ (b*z);
     
           z = Math.pow(b,(emax-2));
           return ((w*z)*b)*b;

     }


    
}