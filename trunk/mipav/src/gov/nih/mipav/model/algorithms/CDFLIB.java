package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.MipavUtil;
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
	
	 private double small;
     private double big;
     private double absstp;
     private double relstp;
     private double stpmul;
     private double abstol;
     private double reltol;
     
     private double xxlo;
     private double xxhi;
     
     private int i99999_dinvr = 0;
     private double fbig_dinvr = 0.0;
     private double fsmall_dinvr = 0.0;
     private double xsave_dinvr = 0.0;
     private double yy_dinvr = 0.0;
     private double step_dinvr = 0.0;
     private double xub_dinvr = 0.0;
     private double xlb_dinvr = 0.0;
     private double xlo_dinvr[] = new double[1];
     private double xhi_dinvr[] = new double[1];
     private boolean qdum1_dinvr[] = new boolean[1];
     private boolean qdum2_dinvr[] = new boolean[1];
     private boolean qup_dinvr;
     private boolean qincr_dinvr = false;
     private boolean qcond_dinvr = false;
     private boolean qlim_dinvr = false;
     private boolean qbdd_dinvr = false;
     
     
     private int i99999_dzror = 0;
     private double fda_dzror,fdb_dzror,m_dzror;
     private double fb_dzror = 0.0;
     private double a_dzror = 0.0;
     private double fa_dzror = 0.0;
     private double b_dzror = 0.0;
     private double c_dzror = 0.0;
     private double fc_dzror = 0.0;
     private double mb_dzror = 0.0;
     private double tol_dzror = 0.0;
     private double d_dzror = 0.0;
     private double fd_dzror = 0.0;
     private double p_dzror = 0.0;
     private double q_dzror = 0.0;
     private double w_dzror = 0.0;
     private int ext_dzror = 0;
     private boolean first_dzror = false;
     private boolean qrzero_dzror;
	
	public CDFLIB() {
		
	}
	
	public void selfTest() {
		int status[] = new int[1];
		double bound[] = new double[1];
		double f[] = new double[1];
		double pi[] = new double[1];
		double qi[] = new double[1];
		double dfni[] = new double[1];
		double dfdi[] = new double[1];
		double p[] = new double[]{0.90,0.95,0.99};
		double q[] = new double[]{0.10,0.05,0.01};
		double dfn[] = new double[19];
		double dfd[] = new double[34];
		int i, j, k;
		for (i = 0; i < 10; i++) {
			dfn[i] = i+1;
		}
		dfn[10] = 12;
		dfn[11] = 15;
		dfn[12] = 20;
		dfn[13] = 24;
		dfn[14] = 30;
		dfn[15] = 40;
		dfn[16] = 60;
		dfn[17] = 120;
		dfn[18] = 9999999;
		for (i = 0; i < 30; i++) {
			dfd[i] = i+1;
		}
		dfd[30] = 40;
		dfd[31] = 60;
		dfd[32] = 120;
		dfd[33] = 9999999;
		double answer[][][] = new double[3][19][34];
		answer[0][0][0] = 39.86;
		answer[0][0][1] = 8.53;
		answer[0][0][2] = 5.54;
		answer[0][0][3] = 4.54;
		answer[0][0][4] = 4.06;
		answer[0][0][5] = 3.78;
		answer[0][0][6] = 3.59;
		answer[0][0][7] = 3.46;
		answer[0][0][8] = 3.36;
		answer[0][0][9] = 3.29;
		answer[0][0][10] = 3.23;
		answer[0][0][11] = 3.18;
		answer[0][0][12] = 3.14;
		answer[0][0][13] = 3.10;
		answer[0][0][14] = 3.07;
		answer[0][0][15] = 3.05;
		answer[0][0][16] = 3.03;
		answer[0][0][17] = 3.01;
		answer[0][0][18] = 2.99;
		answer[0][0][19] = 2.97;
		answer[0][0][20] = 2.96;
		answer[0][0][21] = 2.95;
		answer[0][0][22] = 2.94;
		answer[0][0][23] = 2.93;
		answer[0][0][24] = 2.92;
		answer[0][0][25] = 2.91;
		answer[0][0][26] = 2.90;
		answer[0][0][27] = 2.89;
		answer[0][0][28] = 2.89;
		answer[0][0][29] = 2.88;
		answer[0][0][30] = 2.84;
		answer[0][0][31] = 2.79;
		answer[0][0][32] = 2.75;
		answer[0][0][33] = 2.71;
		answer[0][1][0] = 49.50;
		answer[0][1][1] = 9.00;
		answer[0][1][2] = 5.46;
		answer[0][1][3] = 4.32;
		answer[0][1][4] = 3.78;
		answer[0][1][5] = 3.46;
		answer[0][1][6] = 3.26;
		answer[0][1][7] = 3.11;
		answer[0][1][8] = 3.01;
		answer[0][1][9] = 2.92;
		answer[0][1][10] = 2.86;
		answer[0][1][11] = 2.81;
		answer[0][1][12] = 2.76;
		answer[0][1][13] = 2.73;
		answer[0][1][14] = 2.70;
		answer[0][1][15] = 2.67;
		answer[0][1][16] = 2.64;
		answer[0][1][17] = 2.62;
		answer[0][1][18] = 2.61;
		answer[0][1][19] = 2.59;
		answer[0][1][20] = 2.57;
		answer[0][1][21] = 2.56;
		answer[0][1][22] = 2.55;
		answer[0][1][23] = 2.54;
		answer[0][1][24] = 2.53;
		answer[0][1][25] = 2.52;
		answer[0][1][26] = 2.51;
		answer[0][1][27] = 2.50;
		answer[0][1][28] = 2.50;
		answer[0][1][29] = 2.49;
		answer[0][1][30] = 2.44;
		answer[0][1][31] = 2.39;
		answer[0][1][32] = 2.35;
		answer[0][1][33] = 2.30;
		answer[0][2][0] = 53.59;
		answer[0][2][1] = 9.16;
		answer[0][2][2] = 5.39;
		answer[0][2][3] = 4.19;
		answer[0][2][4] = 3.62;
		answer[0][2][5] = 3.29;
		answer[0][2][6] = 3.07;
		answer[0][2][7] = 2.92;
		answer[0][2][8] = 2.81;
		answer[0][2][9] = 2.73;
		answer[0][2][10] = 2.66;
		answer[0][2][11] = 2.61;
		answer[0][2][12] = 2.56;
		answer[0][2][13] = 2.52;
		answer[0][2][14] = 2.49;
		answer[0][2][15] = 2.46;
		answer[0][2][16] = 2.44;
		answer[0][2][17] = 2.42;
		answer[0][2][18] = 2.40;
		answer[0][2][19] = 2.38;
		answer[0][2][20] = 2.36;
		answer[0][2][21] = 2.35;
		answer[0][2][22] = 2.34;
		answer[0][2][23] = 2.33;
		answer[0][2][24] = 2.32;
		answer[0][2][25] = 2.31;
		answer[0][2][26] = 2.30;
		answer[0][2][27] = 2.29;
		answer[0][2][28] = 2.28;
		answer[0][2][29] = 2.28;
		answer[0][2][30] = 2.23;
		answer[0][2][31] = 2.18;
		answer[0][2][32] = 2.13;
		answer[0][2][33] = 2.08;
		answer[0][3][0] = 55.83;
		answer[0][3][1] = 9.24;
		answer[0][3][2] = 5.34;
		answer[0][3][3] = 4.11;
		answer[0][3][4] = 3.52;
		answer[0][3][5] = 3.18;
		answer[0][3][6] = 2.96;
		answer[0][3][7] = 2.81;
		answer[0][3][8] = 2.69;
		answer[0][3][9] = 2.61;
		answer[0][3][10] = 2.54;
		answer[0][3][11] = 2.48;
		answer[0][3][12] = 2.43;
		answer[0][3][13] = 2.39;
		answer[0][3][14] = 2.36;
		answer[0][3][15] = 2.33;
		answer[0][3][16] = 2.31;
		answer[0][3][17] = 2.29;
		answer[0][3][18] = 2.27;
		answer[0][3][19] = 2.25;
		answer[0][3][20] = 2.23;
		answer[0][3][21] = 2.22;
		answer[0][3][22] = 2.21;
		answer[0][3][23] = 2.19;
		answer[0][3][24] = 2.18;
		answer[0][3][25] = 2.17;
		answer[0][3][26] = 2.17;
		answer[0][3][27] = 2.16;
		answer[0][3][28] = 2.15;
		answer[0][3][29] = 2.14;
		answer[0][3][30] = 2.09;
		answer[0][3][31] = 2.04;
		answer[0][3][32] = 1.99;
		answer[0][3][33] = 1.94;
		answer[0][4][0] = 57.24;
		answer[0][4][1] = 9.29;
		answer[0][4][2] = 5.31;
		answer[0][4][3] = 4.05;
		answer[0][4][4] = 3.45;
		answer[0][4][5] = 3.11;
		answer[0][4][6] = 2.88;
		answer[0][4][7] = 2.73;
		answer[0][4][8] = 2.61;
		answer[0][4][9] = 2.52;
		answer[0][4][10] = 2.45;
		answer[0][4][11] = 2.39;
		answer[0][4][12] = 2.35;
		answer[0][4][13] = 2.31;
		answer[0][4][14] = 2.27;
		answer[0][4][15] = 2.24;
		answer[0][4][16] = 2.22;
		answer[0][4][17] = 2.20;
		answer[0][4][18] = 2.18;
		answer[0][4][19] = 2.16;
		answer[0][4][20] = 2.14;
		answer[0][4][21] = 2.13;
		answer[0][4][22] = 2.11;
		answer[0][4][23] = 2.10;
		answer[0][4][24] = 2.09;
		answer[0][4][25] = 2.08;
		answer[0][4][26] = 2.07;
		answer[0][4][27] = 2.06;
		answer[0][4][28] = 2.06;
		answer[0][4][29] = 2.05;
	    answer[0][4][30] = 2.00;
	    answer[0][4][31] = 1.95;
	    answer[0][4][32] = 1.90;
	    answer[0][4][33] = 1.85;
	    answer[0][5][0] = 58.20;
	    answer[0][5][1] = 9.33;
	    answer[0][5][2] = 5.28;
	    answer[0][5][3] = 4.01;
	    answer[0][5][4] = 3.40;
	    answer[0][5][5] = 3.05;
	    answer[0][5][6] = 2.83;
	    answer[0][5][7] = 2.67;
	    answer[0][5][8] = 2.55;
	    answer[0][5][9] = 2.46;
	    answer[0][5][10] = 2.39;
	    answer[0][5][11] = 2.33;
	    answer[0][5][12] = 2.28;
	    answer[0][5][13] = 2.24;
	    answer[0][5][14] = 2.21;
	    answer[0][5][15] = 2.18;
	    answer[0][5][16] = 2.15;
	    answer[0][5][17] = 2.13;
	    answer[0][5][18] = 2.11;
	    answer[0][5][19] = 2.09;
	    answer[0][5][20] = 2.08;
	    answer[0][5][21] = 2.06;
	    answer[0][5][22] = 2.05;
	    answer[0][5][23] = 2.04;
	    answer[0][5][24] = 2.02;
	    answer[0][5][25] = 2.01;
	    answer[0][5][26] = 2.00;
	    answer[0][5][27] = 2.00;
	    answer[0][5][28] = 1.99;
	    answer[0][5][29] = 1.98;
	    answer[0][5][30] = 1.93;
	    answer[0][5][31] = 1.87;
	    answer[0][5][32] = 1.82;
	    answer[0][5][33] = 1.77;
	    answer[0][6][0] = 58.91;
	    answer[0][6][1] = 9.35;
	    answer[0][6][2] = 5.27;
	    answer[0][6][3] = 3.98;
	    answer[0][6][4] = 3.37;
	    answer[0][6][5] = 3.01;
	    answer[0][6][6] = 2.78;
	    answer[0][6][7] = 2.62;
	    answer[0][6][8] = 2.51;
	    answer[0][6][9] = 2.41;
	    answer[0][6][10] = 2.34;
	    answer[0][6][11] = 2.28;
	    answer[0][6][12] = 2.23;
	    answer[0][6][13] = 2.19;
	    answer[0][6][14] = 2.16;
	    answer[0][6][15] = 2.13;
	    answer[0][6][16] = 2.10;
	    answer[0][6][17] = 2.08;
	    answer[0][6][18] = 2.06;
	    answer[0][6][19] = 2.04;
	    answer[0][6][20] = 2.02;
	    answer[0][6][21] = 2.01;
	    answer[0][6][22] = 1.99;
	    answer[0][6][23] = 1.98;
	    answer[0][6][24] = 1.97;
	    answer[0][6][25] = 1.96;
	    answer[0][6][26] = 1.95;
	    answer[0][6][27] = 1.94;
	    answer[0][6][28] = 1.93;
	    answer[0][6][29] = 1.93;
	    answer[0][6][30] = 1.87;
	    answer[0][6][31] = 1.82;
	    answer[0][6][32] = 1.77;
	    answer[0][6][33] = 1.72;
	    answer[0][7][0] = 59.44;
	    answer[0][7][1] = 9.37;
	    answer[0][7][2] = 5.25;
	    answer[0][7][3] = 3.95;
	    answer[0][7][4] = 3.34;
	    answer[0][7][5] = 2.98;
	    answer[0][7][6] = 2.75;
	    answer[0][7][7] = 2.59;
	    answer[0][7][8] = 2.47;
	    answer[0][7][9] = 2.38;
	    answer[0][7][10] = 2.30;
	    answer[0][7][11] = 2.24;
	    answer[0][7][12] = 2.20;
	    answer[0][7][13] = 2.15;
	    answer[0][7][14] = 2.12;
	    answer[0][7][15] = 2.09;
	    answer[0][7][16] = 2.06;
	    answer[0][7][17] = 2.04;
	    answer[0][7][18] = 2.02;
	    answer[0][7][19] = 2.00;
	    answer[0][7][20] = 1.98;
	    answer[0][7][21] = 1.97;
	    answer[0][7][22] = 1.95;
	    answer[0][7][23] = 1.94;
	    answer[0][7][24] = 1.93;
	    answer[0][7][25] = 1.92;
	    answer[0][7][26] = 1.91;
	    answer[0][7][27] = 1.90;
	    answer[0][7][28] = 1.89;
	    answer[0][7][29] = 1.88;
	    answer[0][7][30] = 1.83;
	    answer[0][7][31] = 1.77;
	    answer[0][7][32] = 1.72;
	    answer[0][7][33] = 1.67;
	    answer[0][8][0] = 59.86;
	    answer[0][8][1] = 9.38;
	    answer[0][8][2] = 5.24;
	    answer[0][8][3] = 3.94;
	    answer[0][8][4] = 3.32;
	    answer[0][8][5] = 2.96;
	    answer[0][8][6] = 2.72;
	    answer[0][8][7] = 2.56;
	    answer[0][8][8] = 2.44;
	    answer[0][8][9] = 2.35;
	    answer[0][8][10] = 2.27;
	    answer[0][8][11] = 2.21;
	    answer[0][8][12] = 2.16;
	    answer[0][8][13] = 2.12;
	    answer[0][8][14] = 2.09;
	    answer[0][8][15] = 2.06;
	    answer[0][8][16] = 2.03;
	    answer[0][8][17] = 2.00;
	    answer[0][8][18] = 1.98;
	    answer[0][8][19] = 1.96;
	    answer[0][8][20] = 1.95;
	    answer[0][8][21] = 1.93;
	    answer[0][8][22] = 1.92;
	    answer[0][8][23] = 1.91;
	    answer[0][8][24] = 1.89;
	    answer[0][8][25] = 1.88;
	    answer[0][8][26] = 1.87;
	    answer[0][8][27] = 1.87;
	    answer[0][8][28] = 1.86;
	    answer[0][8][29] = 1.85;
	    answer[0][8][30] = 1.79;
	    answer[0][8][31] = 1.74;
	    answer[0][8][32] = 1.68;
	    answer[0][8][33] = 1.63;
	    answer[0][9][0] = 60.19;
	    answer[0][9][1] = 9.39;
	    answer[0][9][2] = 5.23;
	    answer[0][9][3] = 3.92;
	    answer[0][9][4] = 3.30;
	    answer[0][9][5] = 2.94;
	    answer[0][9][6] = 2.70;
	    answer[0][9][7] = 2.54;
	    answer[0][9][8] = 2.42;
	    answer[0][9][9] = 2.32;
	    answer[0][9][10] = 2.25;
	    answer[0][9][11] = 2.19;
	    answer[0][9][12] = 2.14;
	    answer[0][9][13] = 2.10;
	    answer[0][9][14] = 2.06;
	    answer[0][9][15] = 2.03;
	    answer[0][9][16] = 2.00;
	    answer[0][9][17] = 1.98;
	    answer[0][9][18] = 1.96;
	    answer[0][9][19] = 1.94;
	    answer[0][9][20] = 1.92;
	    answer[0][9][21] = 1.90;
	    answer[0][9][22] = 1.89;
	    answer[0][9][23] = 1.88;
	    answer[0][9][24] = 1.87;
	    answer[0][9][25] = 1.86;
	    answer[0][9][26] = 1.85;
	    answer[0][9][27] = 1.84;
	    answer[0][9][28] = 1.83;
	    answer[0][9][29] = 1.82;
	    answer[0][9][30] = 1.76;
	    answer[0][9][31] = 1.71;
	    answer[0][9][32] = 1.65;
	    answer[0][9][33] = 1.60;
	    answer[0][10][0] = 60.71;
	    answer[0][10][1] = 9.41;
	    answer[0][10][2] = 5.22;
	    answer[0][10][3] = 3.90;
	    answer[0][10][4] = 3.27;
	    answer[0][10][5] = 2.90;
	    answer[0][10][6] = 2.67;
	    answer[0][10][7] = 2.50;
	    answer[0][10][8] = 2.38;
	    answer[0][10][9] = 2.28;
	    answer[0][10][10] = 2.21;
	    answer[0][10][11] = 2.15;
	    answer[0][10][12] = 2.10;
	    answer[0][10][13] = 2.05;
	    answer[0][10][14] = 2.02;
	    answer[0][10][15] = 1.99;
	    answer[0][10][16] = 1.96;
	    answer[0][10][17] = 1.93;
	    answer[0][10][18] = 1.91;
	    answer[0][10][19] = 1.89;
	    answer[0][10][20] = 1.87;
	    answer[0][10][21] = 1.86;
	    answer[0][10][22] = 1.84;
	    answer[0][10][23] = 1.83;
	    answer[0][10][24] = 1.82;
	    answer[0][10][25] = 1.81;
	    answer[0][10][26] = 1.80;
	    answer[0][10][27] = 1.79;
	    answer[0][10][28] = 1.78;
	    answer[0][10][29] = 1.77;
	    answer[0][10][30] = 1.71;
	    answer[0][10][31] = 1.66;
	    answer[0][10][32] = 1.60;
	    answer[0][10][33] = 1.55;
	    answer[0][11][0] = 61.22;
	    answer[0][11][1] = 9.42;
	    answer[0][11][2] = 5.20;
	    answer[0][11][3] = 3.87;
	    answer[0][11][4] = 3.24;
	    answer[0][11][5] = 2.87;
	    answer[0][11][6] = 2.63;
	    answer[0][11][7] = 2.46;
	    answer[0][11][8] = 2.34;
	    answer[0][11][9] = 2.24;
	    answer[0][11][10] = 2.17;
	    answer[0][11][11] = 2.10;
	    answer[0][11][12] = 2.05;
	    answer[0][11][13] = 2.01;
	    answer[0][11][14] = 1.97;
	    answer[0][11][15] = 1.94;
        answer[0][11][16] = 1.91;
	    answer[0][11][17] = 1.89;
	    answer[0][11][18] = 1.86;
	    answer[0][11][19] = 1.84;
	    answer[0][11][20] = 1.83;
	    answer[0][11][21] = 1.81;
	    answer[0][11][22] = 1.80;
	    answer[0][11][23] = 1.78;
	    answer[0][11][24] = 1.77;
	    answer[0][11][25] = 1.76;
	    answer[0][11][26] = 1.75;
	    answer[0][11][27] = 1.74;
	    answer[0][11][28] = 1.73;
	    answer[0][11][29] = 1.72;
	    answer[0][11][30] = 1.66;
	    answer[0][11][31] = 1.60;
	    answer[0][11][32] = 1.55;
	    answer[0][11][33] = 1.49;
	    answer[0][12][0] = 61.74;
	    answer[0][12][1] = 9.44;
	    answer[0][12][2] = 5.18;
	    answer[0][12][3] = 3.84;
	    answer[0][12][4] = 3.21;
	    answer[0][12][5] = 2.84;
	    answer[0][12][6] = 2.59;
	    answer[0][12][7] = 2.42;
	    answer[0][12][8] = 2.30;
	    answer[0][12][9] = 2.20;
	    answer[0][12][10] = 2.12;
	    answer[0][12][11] = 2.06;
	    answer[0][12][12] = 2.01;
	    answer[0][12][13] = 1.96;
	    answer[0][12][14] = 1.92;
	    answer[0][12][15] = 1.89;
        answer[0][12][16] = 1.86;
	    answer[0][12][17] = 1.84;
	    answer[0][12][18] = 1.81;
	    answer[0][12][19] = 1.79;
	    answer[0][12][20] = 1.78;
	    answer[0][12][21] = 1.76;
	    answer[0][12][22] = 1.74;
	    answer[0][12][23] = 1.73;
	    answer[0][12][24] = 1.72;
	    answer[0][12][25] = 1.71;
	    answer[0][12][26] = 1.70;
	    answer[0][12][27] = 1.69;
	    answer[0][12][28] = 1.68;
	    answer[0][12][29] = 1.67;
	    answer[0][12][30] = 1.61;
	    answer[0][12][31] = 1.54;
	    answer[0][12][32] = 1.48;
	    answer[0][12][33] = 1.42;
	    answer[0][13][0] = 62.00;
	    answer[0][13][1] = 9.45;
	    answer[0][13][2] = 5.18;
	    answer[0][13][3] = 3.83;
	    answer[0][13][4] = 3.19;
	    answer[0][13][5] = 2.82;
	    answer[0][13][6] = 2.58;
	    answer[0][13][7] = 2.40;
	    answer[0][13][8] = 2.28;
	    answer[0][13][9] = 2.18;
	    answer[0][13][10] = 2.10;
	    answer[0][13][11] = 2.04;
	    answer[0][13][12] = 1.98;
	    answer[0][13][13] = 1.94;
	    answer[0][13][14] = 1.90;
	    answer[0][13][15] = 1.87;
        answer[0][13][16] = 1.84;
	    answer[0][13][17] = 1.81;
	    answer[0][13][18] = 1.79;
	    answer[0][13][19] = 1.77;
	    answer[0][13][20] = 1.75;
	    answer[0][13][21] = 1.73;
	    answer[0][13][22] = 1.72;
	    answer[0][13][23] = 1.70;
	    answer[0][13][24] = 1.69;
	    answer[0][13][25] = 1.68;
	    answer[0][13][26] = 1.67;
	    answer[0][13][27] = 1.66;
	    answer[0][13][28] = 1.65;
	    answer[0][13][29] = 1.64;
	    answer[0][13][30] = 1.57;
	    answer[0][13][31] = 1.51;
	    answer[0][13][32] = 1.45;
	    answer[0][13][33] = 1.38;
	    answer[0][14][0] = 62.26;
	    answer[0][14][1] = 9.46;
	    answer[0][14][2] = 5.17;
	    answer[0][14][3] = 3.82;
	    answer[0][14][4] = 3.17;
	    answer[0][14][5] = 2.80;
	    answer[0][14][6] = 2.56;
	    answer[0][14][7] = 2.38;
	    answer[0][14][8] = 2.25;
	    answer[0][14][9] = 2.16;
	    answer[0][14][10] = 2.08;
	    answer[0][14][11] = 2.01;
	    answer[0][14][12] = 1.96;
	    answer[0][14][13] = 1.91;
	    answer[0][14][14] = 1.87;
	    answer[0][14][15] = 1.84;
        answer[0][14][16] = 1.81;
	    answer[0][14][17] = 1.78;
	    answer[0][14][18] = 1.76;
	    answer[0][14][19] = 1.74;
	    answer[0][14][20] = 1.72;
	    answer[0][14][21] = 1.70;
	    answer[0][14][22] = 1.69;
	    answer[0][14][23] = 1.67;
	    answer[0][14][24] = 1.66;
	    answer[0][14][25] = 1.65;
	    answer[0][14][26] = 1.64;
	    answer[0][14][27] = 1.63;
	    answer[0][14][28] = 1.62;
	    answer[0][14][29] = 1.61;
	    answer[0][14][30] = 1.54;
	    answer[0][14][31] = 1.48;
	    answer[0][14][32] = 1.41;
	    answer[0][14][33] = 1.34;
	    answer[0][15][0] = 62.53;
	    answer[0][15][1] = 9.47;
	    answer[0][15][2] = 5.16;
	    answer[0][15][3] = 3.80;
	    answer[0][15][4] = 3.16;
	    answer[0][15][5] = 2.78;
	    answer[0][15][6] = 2.54;
	    answer[0][15][7] = 2.36;
	    answer[0][15][8] = 2.23;
	    answer[0][15][9] = 2.13;
	    answer[0][15][10] = 2.05;
	    answer[0][15][11] = 1.99;
	    answer[0][15][12] = 1.93;
	    answer[0][15][13] = 1.89;
	    answer[0][15][14] = 1.85;
	    answer[0][15][15] = 1.81;
        answer[0][15][16] = 1.78;
	    answer[0][15][17] = 1.75;
	    answer[0][15][18] = 1.73;
	    answer[0][15][19] = 1.71;
	    answer[0][15][20] = 1.69;
	    answer[0][15][21] = 1.67;
	    answer[0][15][22] = 1.66;
	    answer[0][15][23] = 1.64;
	    answer[0][15][24] = 1.63;
	    answer[0][15][25] = 1.61;
	    answer[0][15][26] = 1.60;
	    answer[0][15][27] = 1.59;
	    answer[0][15][28] = 1.58;
	    answer[0][15][29] = 1.57;
	    answer[0][15][30] = 1.51;
	    answer[0][15][31] = 1.44;
	    answer[0][15][32] = 1.37;
	    answer[0][15][33] = 1.30;
	    answer[0][16][0] = 62.79;
	    answer[0][16][1] = 9.47;
	    answer[0][16][2] = 5.15;
	    answer[0][16][3] = 3.79;
	    answer[0][16][4] = 3.14;
	    answer[0][16][5] = 2.76;
	    answer[0][16][6] = 2.51;
	    answer[0][16][7] = 2.34;
	    answer[0][16][8] = 2.21;
	    answer[0][16][9] = 2.11;
	    answer[0][16][10] = 2.03;
	    answer[0][16][11] = 1.96;
	    answer[0][16][12] = 1.90;
	    answer[0][16][13] = 1.86;
	    answer[0][16][14] = 1.82;
	    answer[0][16][15] = 1.78;
        answer[0][16][16] = 1.75;
	    answer[0][16][17] = 1.72;
	    answer[0][16][18] = 1.70;
	    answer[0][16][19] = 1.68;
	    answer[0][16][20] = 1.66;
	    answer[0][16][21] = 1.64;
	    answer[0][16][22] = 1.62;
	    answer[0][16][23] = 1.61;
	    answer[0][16][24] = 1.59;
	    answer[0][16][25] = 1.58;
	    answer[0][16][26] = 1.57;
	    answer[0][16][27] = 1.56;
	    answer[0][16][28] = 1.55;
	    answer[0][16][29] = 1.54;
	    answer[0][16][30] = 1.47;
	    answer[0][16][31] = 1.40;
	    answer[0][16][32] = 1.32;
	    answer[0][16][33] = 1.24;
	    answer[0][17][0] = 63.06;
	    answer[0][17][1] = 9.48;
	    answer[0][17][2] = 5.14;
	    answer[0][17][3] = 3.78;
	    answer[0][17][4] = 3.12;
	    answer[0][17][5] = 2.74;
	    answer[0][17][6] = 2.49;
	    answer[0][17][7] = 2.32;
	    answer[0][17][8] = 2.18;
	    answer[0][17][9] = 2.08;
	    answer[0][17][10] = 2.00;
	    answer[0][17][11] = 1.93;
	    answer[0][17][12] = 1.88;
	    answer[0][17][13] = 1.83;
	    answer[0][17][14] = 1.79;
	    answer[0][17][15] = 1.75;
        answer[0][17][16] = 1.72;
	    answer[0][17][17] = 1.69;
	    answer[0][17][18] = 1.67;
	    answer[0][17][19] = 1.64;
	    answer[0][17][20] = 1.62;
	    answer[0][17][21] = 1.60;
	    answer[0][17][22] = 1.59;
	    answer[0][17][23] = 1.57;
	    answer[0][17][24] = 1.56;
	    answer[0][17][25] = 1.54;
	    answer[0][17][26] = 1.53;
	    answer[0][17][27] = 1.52;
	    answer[0][17][28] = 1.51;
	    answer[0][17][29] = 1.50;
	    answer[0][17][30] = 1.42;
	    answer[0][17][31] = 1.35;
	    answer[0][17][32] = 1.26;
	    answer[0][17][33] = 1.17;
	    answer[0][18][0] = 63.33;
	    answer[0][18][1] = 9.49;
	    answer[0][18][2] = 5.13;
	    answer[0][18][3] = 3.76;
	    answer[0][18][4] = 3.10;
	    answer[0][18][5] = 2.72;
	    answer[0][18][6] = 2.47;
	    answer[0][18][7] = 2.29;
	    answer[0][18][8] = 2.16;
	    answer[0][18][9] = 2.06;
	    answer[0][18][10] = 1.97;
	    answer[0][18][11] = 1.90;
	    answer[0][18][12] = 1.85;
	    answer[0][18][13] = 1.80;
	    answer[0][18][14] = 1.76;
	    answer[0][18][15] = 1.72;
        answer[0][18][16] = 1.69;
	    answer[0][18][17] = 1.66;
	    answer[0][18][18] = 1.63;
	    answer[0][18][19] = 1.61;
	    answer[0][18][20] = 1.59;
	    answer[0][18][21] = 1.57;
	    answer[0][18][22] = 1.55;
	    answer[0][18][23] = 1.53;
	    answer[0][18][24] = 1.52;
	    answer[0][18][25] = 1.50;
	    answer[0][18][26] = 1.49;
	    answer[0][18][27] = 1.48;
	    answer[0][18][28] = 1.47;
	    answer[0][18][29] = 1.46;
	    answer[0][18][30] = 1.38;
	    answer[0][18][31] = 1.29;
	    answer[0][18][32] = 1.19;
	    answer[0][18][33] = 1.00;
	    answer[1][0][0] = 161.4;
		answer[1][0][1] = 18.51;
		answer[1][0][2] = 10.13;
		answer[1][0][3] = 7.71;
		answer[1][0][4] = 6.61;
		answer[1][0][5] = 5.99;
		answer[1][0][6] = 5.59;
		answer[1][0][7] = 5.32;
		answer[1][0][8] = 5.12;
		answer[1][0][9] = 4.96;
		answer[1][0][10] = 4.84;
		answer[1][0][11] = 4.75;
		answer[1][0][12] = 4.67;
		answer[1][0][13] = 4.60;
		answer[1][0][14] = 4.54;
		answer[1][0][15] = 4.49;
		answer[1][0][16] = 4.45;
		answer[1][0][17] = 4.41;
		answer[1][0][18] = 4.38;
		answer[1][0][19] = 4.35;
		answer[1][0][20] = 4.32;
		answer[1][0][21] = 4.30;
		answer[1][0][22] = 4.28;
		answer[1][0][23] = 4.26;
		answer[1][0][24] = 4.24;
		answer[1][0][25] = 4.23;
		answer[1][0][26] = 4.21;
		answer[1][0][27] = 4.20;
		answer[1][0][28] = 4.18;
		answer[1][0][29] = 4.17;
		answer[1][0][30] = 4.08;
		answer[1][0][31] = 4.00;
		answer[1][0][32] = 3.92;
		answer[1][0][33] = 3.84;
		answer[1][1][0] = 199.5;
		answer[1][1][1] = 19.00;
		answer[1][1][2] = 9.55;
		answer[1][1][3] = 6.94;
		answer[1][1][4] = 5.79;
		answer[1][1][5] = 5.14;
		answer[1][1][6] = 4.74;
		answer[1][1][7] = 4.46;
		answer[1][1][8] = 4.26;
		answer[1][1][9] = 4.10;
		answer[1][1][10] = 3.98;
		answer[1][1][11] = 3.89;
		answer[1][1][12] = 3.81;
		answer[1][1][13] = 3.74;
		answer[1][1][14] = 3.68;
		answer[1][1][15] = 3.63;
		answer[1][1][16] = 3.59;
		answer[1][1][17] = 3.55;
		answer[1][1][18] = 3.52;
		answer[1][1][19] = 3.49;
		answer[1][1][20] = 3.47;
		answer[1][1][21] = 3.44;
		answer[1][1][22] = 3.42;
		answer[1][1][23] = 3.40;
		answer[1][1][24] = 3.39;
		answer[1][1][25] = 3.37;
		answer[1][1][26] = 3.35;
		answer[1][1][27] = 3.34;
		answer[1][1][28] = 3.33;
		answer[1][1][29] = 3.32;
		answer[1][1][30] = 3.23;
		answer[1][1][31] = 3.15;
		answer[1][1][32] = 3.07;
		answer[1][1][33] = 3.00;
		answer[1][2][0] = 215.7;
		answer[1][2][1] = 19.16;
		answer[1][2][2] = 9.28;
		answer[1][2][3] = 6.59;
		answer[1][2][4] = 5.41;
		answer[1][2][5] = 4.76;
		answer[1][2][6] = 4.35;
		answer[1][2][7] = 4.07;
		answer[1][2][8] = 3.86;
		answer[1][2][9] = 3.71;
		answer[1][2][10] = 3.59;
		answer[1][2][11] = 3.49;
		answer[1][2][12] = 3.41;
		answer[1][2][13] = 3.34;
		answer[1][2][14] = 3.29;
		answer[1][2][15] = 3.24;
		answer[1][2][16] = 3.20;
		answer[1][2][17] = 3.16;
		answer[1][2][18] = 3.13;
		answer[1][2][19] = 3.10;
		answer[1][2][20] = 3.07;
		answer[1][2][21] = 3.05;
		answer[1][2][22] = 3.03;
		answer[1][2][23] = 3.01;
		answer[1][2][24] = 2.99;
		answer[1][2][25] = 2.98;
		answer[1][2][26] = 2.96;
		answer[1][2][27] = 2.95;
		answer[1][2][28] = 2.93;
		answer[1][2][29] = 2.92;
		answer[1][2][30] = 2.84;
		answer[1][2][31] = 2.76;
		answer[1][2][32] = 2.68;
		answer[1][2][33] = 2.60;
		answer[1][3][0] = 224.6;
		answer[1][3][1] = 19.25;
		answer[1][3][2] = 9.12;
		answer[1][3][3] = 6.39; 
		answer[1][3][4] = 5.19;
		answer[1][3][5] = 4.53;
		answer[1][3][6] = 4.12;
		answer[1][3][7] = 3.84;
		answer[1][3][8] = 3.63;
		answer[1][3][9] = 3.48;
		answer[1][3][10] = 3.36;
		answer[1][3][11] = 3.26;
		answer[1][3][12] = 3.18;
		answer[1][3][13] = 3.11;
		answer[1][3][14] = 3.06;
		answer[1][3][15] = 3.01;
		answer[1][3][16] = 2.96;
		answer[1][3][17] = 2.93;
		answer[1][3][18] = 2.90;
		answer[1][3][19] = 2.87;
		answer[1][3][20] = 2.84;
		answer[1][3][21] = 2.82;
		answer[1][3][22] = 2.80;
		answer[1][3][23] = 2.78;
		answer[1][3][24] = 2.76;
		answer[1][3][25] = 2.74;
		answer[1][3][26] = 2.73;
		answer[1][3][27] = 2.71;
		answer[1][3][28] = 2.70;
		answer[1][3][29] = 2.69;
		answer[1][3][30] = 2.61;
		answer[1][3][31] = 2.53;
		answer[1][3][32] = 2.45;
		answer[1][3][33] = 2.37;
		answer[1][4][0] = 230.2;
		answer[1][4][1] = 19.30;
		answer[1][4][2] = 9.01;
		answer[1][4][3] = 6.26;
		answer[1][4][4] = 5.05;
		answer[1][4][5] = 4.39;
		answer[1][4][6] = 3.97;
		answer[1][4][7] = 3.69;
		answer[1][4][8] = 3.48;
		answer[1][4][9] = 3.33;
		answer[1][4][10] = 3.20;
		answer[1][4][11] = 3.11;
		answer[1][4][12] = 3.03;
		answer[1][4][13] = 2.96;
		answer[1][4][14] = 2.90;
		answer[1][4][15] = 2.85;
		answer[1][4][16] = 2.81;
		answer[1][4][17] = 2.77;
		answer[1][4][18] = 2.74;
		answer[1][4][19] = 2.71;
		answer[1][4][20] = 2.68;
		answer[1][4][21] = 2.66;
		answer[1][4][22] = 2.64;
		answer[1][4][23] = 2.62;
		answer[1][4][24] = 2.60;
		answer[1][4][25] = 2.59;
		answer[1][4][26] = 2.57;
		answer[1][4][27] = 2.56;
		answer[1][4][28] = 2.55;
		answer[1][4][29] = 2.53;
	    answer[1][4][30] = 2.45;
	    answer[1][4][31] = 2.37;
	    answer[1][4][32] = 2.29;
	    answer[1][4][33] = 2.21;
	    answer[1][5][0] = 234.0;
	    answer[1][5][1] = 19.33;
	    answer[1][5][2] = 8.94;
	    answer[1][5][3] = 6.16;
	    answer[1][5][4] = 4.95;
	    answer[1][5][5] = 4.28;
	    answer[1][5][6] = 3.87;
	    answer[1][5][7] = 3.58;
	    answer[1][5][8] = 3.37;
	    answer[1][5][9] = 3.22;
	    answer[1][5][10] = 3.09;
	    answer[1][5][11] = 3.00;
	    answer[1][5][12] = 2.92;
	    answer[1][5][13] = 2.85;
	    answer[1][5][14] = 2.79;
	    answer[1][5][15] = 2.74;
	    answer[1][5][16] = 2.70;
	    answer[1][5][17] = 2.66;
	    answer[1][5][18] = 2.63;
	    answer[1][5][19] = 2.60;
	    answer[1][5][20] = 2.57;
	    answer[1][5][21] = 2.55;
	    answer[1][5][22] = 2.53;
	    answer[1][5][23] = 2.51;
	    answer[1][5][24] = 2.49;
	    answer[1][5][25] = 2.47;
	    answer[1][5][26] = 2.46;
	    answer[1][5][27] = 2.45;
	    answer[1][5][28] = 2.43;
	    answer[1][5][29] = 2.42;
	    answer[1][5][30] = 2.34;
	    answer[1][5][31] = 2.25;
	    answer[1][5][32] = 2.17;
	    answer[1][5][33] = 2.10;
	    answer[1][6][0] = 236.8;
	    answer[1][6][1] = 19.35;
	    answer[1][6][2] = 8.89;
	    answer[1][6][3] = 6.09;
	    answer[1][6][4] = 4.88;
	    answer[1][6][5] = 4.21;
	    answer[1][6][6] = 3.79;
	    answer[1][6][7] = 3.50;
	    answer[1][6][8] = 3.29;
	    answer[1][6][9] = 3.14;
	    answer[1][6][10] = 3.01;
	    answer[1][6][11] = 2.91;
	    answer[1][6][12] = 2.83;
	    answer[1][6][13] = 2.76;
	    answer[1][6][14] = 2.71;
	    answer[1][6][15] = 2.66;
	    answer[1][6][16] = 2.61;
	    answer[1][6][17] = 2.58;
	    answer[1][6][18] = 2.54;
	    answer[1][6][19] = 2.51;
	    answer[1][6][20] = 2.49;
	    answer[1][6][21] = 2.46;
	    answer[1][6][22] = 2.44;
	    answer[1][6][23] = 2.42;
	    answer[1][6][24] = 2.40;
	    answer[1][6][25] = 2.39;
	    answer[1][6][26] = 2.37;
	    answer[1][6][27] = 2.36;
	    answer[1][6][28] = 2.35;
	    answer[1][6][29] = 2.33;
	    answer[1][6][30] = 2.25;
	    answer[1][6][31] = 2.17;
	    answer[1][6][32] = 2.09;
	    answer[1][6][33] = 2.01;
	    answer[1][7][0] = 238.9;
	    answer[1][7][1] = 19.37;
	    answer[1][7][2] = 8.85;
	    answer[1][7][3] = 6.04;
	    answer[1][7][4] = 4.82;
	    answer[1][7][5] = 4.15;
	    answer[1][7][6] = 3.73;
	    answer[1][7][7] = 3.44;
	    answer[1][7][8] = 3.23;
	    answer[1][7][9] = 3.07;
	    answer[1][7][10] = 2.95;
	    answer[1][7][11] = 2.85;
	    answer[1][7][12] = 2.77;
	    answer[1][7][13] = 2.70;
	    answer[1][7][14] = 2.64;
	    answer[1][7][15] = 2.59;
	    answer[1][7][16] = 2.55;
	    answer[1][7][17] = 2.51;
	    answer[1][7][18] = 2.48;
	    answer[1][7][19] = 2.45;
	    answer[1][7][20] = 2.42;
	    answer[1][7][21] = 2.40;
	    answer[1][7][22] = 2.37;
	    answer[1][7][23] = 2.36;
	    answer[1][7][24] = 2.34;
	    answer[1][7][25] = 2.32;
	    answer[1][7][26] = 2.31;
	    answer[1][7][27] = 2.29;
	    answer[1][7][28] = 2.28;
	    answer[1][7][29] = 2.27;
	    answer[1][7][30] = 2.18;
	    answer[1][7][31] = 2.10;
	    answer[1][7][32] = 2.02;
	    answer[1][7][33] = 1.94;
	    answer[1][8][0] = 240.5;
	    answer[1][8][1] = 19.38;
	    answer[1][8][2] = 8.81;
	    answer[1][8][3] = 6.00;
	    answer[1][8][4] = 4.77;
	    answer[1][8][5] = 4.10;
	    answer[1][8][6] = 3.68;
	    answer[1][8][7] = 3.39;
	    answer[1][8][8] = 3.18;
	    answer[1][8][9] = 3.02;
	    answer[1][8][10] = 2.90;
	    answer[1][8][11] = 2.80;
	    answer[1][8][12] = 2.71;
	    answer[1][8][13] = 2.65;
	    answer[1][8][14] = 2.59;
	    answer[1][8][15] = 2.54;
	    answer[1][8][16] = 2.49;
	    answer[1][8][17] = 2.46;
	    answer[1][8][18] = 2.42;
	    answer[1][8][19] = 2.39;
	    answer[1][8][20] = 2.37;
	    answer[1][8][21] = 2.34;
	    answer[1][8][22] = 2.32;
	    answer[1][8][23] = 2.30;
	    answer[1][8][24] = 2.28;
	    answer[1][8][25] = 2.27;
	    answer[1][8][26] = 2.25;
	    answer[1][8][27] = 2.24;
	    answer[1][8][28] = 2.22;
	    answer[1][8][29] = 2.21;
	    answer[1][8][30] = 2.12;
	    answer[1][8][31] = 2.04;
	    answer[1][8][32] = 1.96;
	    answer[1][8][33] = 1.88;
	    answer[1][9][0] = 241.9;
	    answer[1][9][1] = 19.40;
	    answer[1][9][2] = 8.79;
	    answer[1][9][3] = 5.96;
	    answer[1][9][4] = 4.74;
	    answer[1][9][5] = 4.06;
	    answer[1][9][6] = 3.64;
	    answer[1][9][7] = 3.35;
	    answer[1][9][8] = 3.14;
	    answer[1][9][9] = 2.98;
	    answer[1][9][10] = 2.85;
	    answer[1][9][11] = 2.75;
	    answer[1][9][12] = 2.67;
	    answer[1][9][13] = 2.60;
	    answer[1][9][14] = 2.54;
	    answer[1][9][15] = 2.49;
	    answer[1][9][16] = 2.45;
	    answer[1][9][17] = 2.41;
	    answer[1][9][18] = 2.38;
	    answer[1][9][19] = 2.35;
	    answer[1][9][20] = 2.32;
	    answer[1][9][21] = 2.30;
	    answer[1][9][22] = 2.27;
	    answer[1][9][23] = 2.25;
	    answer[1][9][24] = 2.24;
	    answer[1][9][25] = 2.22;
	    answer[1][9][26] = 2.20;
	    answer[1][9][27] = 2.19;
	    answer[1][9][28] = 2.18;
	    answer[1][9][29] = 2.16;
	    answer[1][9][30] = 2.08;
	    answer[1][9][31] = 1.99;
	    answer[1][9][32] = 1.91;
	    answer[1][9][33] = 1.83;
	    answer[1][10][0] = 243.9;
	    answer[1][10][1] = 19.41;
	    answer[1][10][2] = 8.74;
	    answer[1][10][3] = 5.91;
	    answer[1][10][4] = 4.68;
	    answer[1][10][5] = 4.00;
	    answer[1][10][6] = 3.57;
	    answer[1][10][7] = 3.28;
	    answer[1][10][8] = 3.07;
	    answer[1][10][9] = 2.91;
	    answer[1][10][10] = 2.79;
	    answer[1][10][11] = 2.69;
	    answer[1][10][12] = 2.60;
	    answer[1][10][13] = 2.53;
	    answer[1][10][14] = 2.48;
	    answer[1][10][15] = 2.42;
	    answer[1][10][16] = 2.38;
	    answer[1][10][17] = 2.34;
	    answer[1][10][18] = 2.31;
	    answer[1][10][19] = 2.28;
	    answer[1][10][20] = 2.25;
	    answer[1][10][21] = 2.23;
	    answer[1][10][22] = 2.20;
	    answer[1][10][23] = 2.18;
	    answer[1][10][24] = 2.16;
	    answer[1][10][25] = 2.15;
	    answer[1][10][26] = 2.13;
	    answer[1][10][27] = 2.12;
	    answer[1][10][28] = 2.10;
	    answer[1][10][29] = 2.09;
	    answer[1][10][30] = 2.00;
	    answer[1][10][31] = 1.92;
	    answer[1][10][32] = 1.83;
	    answer[1][10][33] = 1.75;
	    answer[1][11][0] = 245.9;
	    answer[1][11][1] = 19.43;
	    answer[1][11][2] = 8.70;
	    answer[1][11][3] = 5.86;
	    answer[1][11][4] = 4.62;
	    answer[1][11][5] = 3.94;
	    answer[1][11][6] = 3.51;
	    answer[1][11][7] = 3.22;
	    answer[1][11][8] = 3.01;
	    answer[1][11][9] = 2.85;
	    answer[1][11][10] = 2.72;
	    answer[1][11][11] = 2.62;
	    answer[1][11][12] = 2.53;
	    answer[1][11][13] = 2.46;
	    answer[1][11][14] = 2.40;
	    answer[1][11][15] = 2.35;
        answer[1][11][16] = 2.31;
	    answer[1][11][17] = 2.27;
	    answer[1][11][18] = 2.23;
	    answer[1][11][19] = 2.20;
	    answer[1][11][20] = 2.18;
	    answer[1][11][21] = 2.15;
	    answer[1][11][22] = 2.13;
	    answer[1][11][23] = 2.11;
	    answer[1][11][24] = 2.09;
	    answer[1][11][25] = 2.07;
	    answer[1][11][26] = 2.06;
	    answer[1][11][27] = 2.04;
	    answer[1][11][28] = 2.03;
	    answer[1][11][29] = 2.01;
	    answer[1][11][30] = 1.92;
	    answer[1][11][31] = 1.84;
	    answer[1][11][32] = 1.75;
	    answer[1][11][33] = 1.67;
	    answer[1][12][0] = 248.0;
	    answer[1][12][1] = 19.45;
	    answer[1][12][2] = 8.66;
	    answer[1][12][3] = 5.80;
	    answer[1][12][4] = 4.56;
	    answer[1][12][5] = 3.87;
	    answer[1][12][6] = 3.44;
	    answer[1][12][7] = 3.15;
	    answer[1][12][8] = 2.94;
	    answer[1][12][9] = 2.77;
	    answer[1][12][10] = 2.65;
	    answer[1][12][11] = 2.54;
	    answer[1][12][12] = 2.46;
	    answer[1][12][13] = 2.39;
	    answer[1][12][14] = 2.33;
	    answer[1][12][15] = 2.28;
        answer[1][12][16] = 2.23;
	    answer[1][12][17] = 2.19;
	    answer[1][12][18] = 2.16;
	    answer[1][12][19] = 2.12;
	    answer[1][12][20] = 2.10;
	    answer[1][12][21] = 2.07;
	    answer[1][12][22] = 2.05;
	    answer[1][12][23] = 2.03;
	    answer[1][12][24] = 2.01;
	    answer[1][12][25] = 1.99;
	    answer[1][12][26] = 1.97;
	    answer[1][12][27] = 1.96;
	    answer[1][12][28] = 1.94;
	    answer[1][12][29] = 1.93;
	    answer[1][12][30] = 1.84;
	    answer[1][12][31] = 1.75;
	    answer[1][12][32] = 1.66;
	    answer[1][12][33] = 1.57;
	    answer[1][13][0] = 249.1;
	    answer[1][13][1] = 19.45;
	    answer[1][13][2] = 8.64;
	    answer[1][13][3] = 5.77;
	    answer[1][13][4] = 4.53;
	    answer[1][13][5] = 3.84;
	    answer[1][13][6] = 3.41;
	    answer[1][13][7] = 3.12;
	    answer[1][13][8] = 2.90;
	    answer[1][13][9] = 2.74;
	    answer[1][13][10] = 2.61;
	    answer[1][13][11] = 2.51;
	    answer[1][13][12] = 2.42;
	    answer[1][13][13] = 2.35;
	    answer[1][13][14] = 2.29;
	    answer[1][13][15] = 2.24;
        answer[1][13][16] = 2.19;
	    answer[1][13][17] = 2.15;
	    answer[1][13][18] = 2.11;
	    answer[1][13][19] = 2.08;
	    answer[1][13][20] = 2.05;
	    answer[1][13][21] = 2.03;
	    answer[1][13][22] = 2.01;
	    answer[1][13][23] = 1.98;
	    answer[1][13][24] = 1.96;
	    answer[1][13][25] = 1.95;
	    answer[1][13][26] = 1.93;
	    answer[1][13][27] = 1.91;
	    answer[1][13][28] = 1.90;
	    answer[1][13][29] = 1.89;
	    answer[1][13][30] = 1.79;
	    answer[1][13][31] = 1.70;
	    answer[1][13][32] = 1.61;
	    answer[1][13][33] = 1.52;
	    answer[1][14][0] = 250.1;
	    answer[1][14][1] = 19.46;
	    answer[1][14][2] = 8.62;
	    answer[1][14][3] = 5.75;
	    answer[1][14][4] = 4.50;
	    answer[1][14][5] = 3.81;
	    answer[1][14][6] = 3.38;
	    answer[1][14][7] = 3.08;
	    answer[1][14][8] = 2.86;
	    answer[1][14][9] = 2.70;
	    answer[1][14][10] = 2.57;
	    answer[1][14][11] = 2.47;
	    answer[1][14][12] = 2.38;
	    answer[1][14][13] = 2.31;
	    answer[1][14][14] = 2.25;
	    answer[1][14][15] = 2.19;
        answer[1][14][16] = 2.15;
	    answer[1][14][17] = 2.11;
	    answer[1][14][18] = 2.07;
	    answer[1][14][19] = 2.04;
	    answer[1][14][20] = 2.01;
	    answer[1][14][21] = 1.98;
	    answer[1][14][22] = 1.96;
	    answer[1][14][23] = 1.94;
	    answer[1][14][24] = 1.92;
	    answer[1][14][25] = 1.90;
	    answer[1][14][26] = 1.88;
	    answer[1][14][27] = 1.87;
	    answer[1][14][28] = 1.85;
	    answer[1][14][29] = 1.84;
	    answer[1][14][30] = 1.74;
	    answer[1][14][31] = 1.65;
	    answer[1][14][32] = 1.55;
	    answer[1][14][33] = 1.46;
	    answer[1][15][0] = 251.1;
	    answer[1][15][1] = 19.47;
	    answer[1][15][2] = 8.59;
	    answer[1][15][3] = 5.72;
	    answer[1][15][4] = 4.46;
	    answer[1][15][5] = 3.77;
	    answer[1][15][6] = 3.34;
	    answer[1][15][7] = 3.04;
	    answer[1][15][8] = 2.83;
	    answer[1][15][9] = 2.66;
	    answer[1][15][10] = 2.53;
	    answer[1][15][11] = 2.43;
	    answer[1][15][12] = 2.34;
	    answer[1][15][13] = 2.27;
	    answer[1][15][14] = 2.20;
	    answer[1][15][15] = 2.15;
        answer[1][15][16] = 2.10;
	    answer[1][15][17] = 2.06;
	    answer[1][15][18] = 2.03;
	    answer[1][15][19] = 1.99;
	    answer[1][15][20] = 1.96;
	    answer[1][15][21] = 1.94;
	    answer[1][15][22] = 1.91;
	    answer[1][15][23] = 1.89;
	    answer[1][15][24] = 1.87;
	    answer[1][15][25] = 1.85;
	    answer[1][15][26] = 1.84;
	    answer[1][15][27] = 1.82;
	    answer[1][15][28] = 1.81;
	    answer[1][15][29] = 1.79;
	    answer[1][15][30] = 1.69;
	    answer[1][15][31] = 1.59;
	    answer[1][15][32] = 1.50;
	    answer[1][15][33] = 1.39;
	    answer[1][16][0] = 252.2;
	    answer[1][16][1] = 19.48;
	    answer[1][16][2] = 8.57;
	    answer[1][16][3] = 5.69;
	    answer[1][16][4] = 4.43;
	    answer[1][16][5] = 3.74;
	    answer[1][16][6] = 3.30;
	    answer[1][16][7] = 3.01;
	    answer[1][16][8] = 2.79;
	    answer[1][16][9] = 2.62;
	    answer[1][16][10] = 2.49;
	    answer[1][16][11] = 2.38;
	    answer[1][16][12] = 2.30;
	    answer[1][16][13] = 2.22;
	    answer[1][16][14] = 2.16;
	    answer[1][16][15] = 2.11;
        answer[1][16][16] = 2.06;
	    answer[1][16][17] = 2.02;
	    answer[1][16][18] = 1.98;
	    answer[1][16][19] = 1.95;
	    answer[1][16][20] = 1.92;
	    answer[1][16][21] = 1.89;
	    answer[1][16][22] = 1.86;
	    answer[1][16][23] = 1.84;
	    answer[1][16][24] = 1.82;
	    answer[1][16][25] = 1.80;
	    answer[1][16][26] = 1.79;
	    answer[1][16][27] = 1.77;
	    answer[1][16][28] = 1.75;
	    answer[1][16][29] = 1.74;
	    answer[1][16][30] = 1.64;
	    answer[1][16][31] = 1.53;
	    answer[1][16][32] = 1.43;
	    answer[1][16][33] = 1.32;
	    answer[1][17][0] = 253.3;
	    answer[1][17][1] = 19.49;
	    answer[1][17][2] = 8.55;
	    answer[1][17][3] = 5.66;
	    answer[1][17][4] = 4.40;
	    answer[1][17][5] = 3.70;
	    answer[1][17][6] = 3.27;
	    answer[1][17][7] = 2.97;
	    answer[1][17][8] = 2.75;
	    answer[1][17][9] = 2.58;
	    answer[1][17][10] = 2.45;
	    answer[1][17][11] = 2.34;
	    answer[1][17][12] = 2.25;
	    answer[1][17][13] = 2.18;
	    answer[1][17][14] = 2.11;
	    answer[1][17][15] = 2.06;
        answer[1][17][16] = 2.01;
	    answer[1][17][17] = 1.97;
	    answer[1][17][18] = 1.93;
	    answer[1][17][19] = 1.90;
	    answer[1][17][20] = 1.87;
	    answer[1][17][21] = 1.84;
	    answer[1][17][22] = 1.81;
	    answer[1][17][23] = 1.79;
	    answer[1][17][24] = 1.77;
	    answer[1][17][25] = 1.75;
	    answer[1][17][26] = 1.73;
	    answer[1][17][27] = 1.71;
	    answer[1][17][28] = 1.70;
	    answer[1][17][29] = 1.68;
	    answer[1][17][30] = 1.58;
	    answer[1][17][31] = 1.47;
	    answer[1][17][32] = 1.35;
	    answer[1][17][33] = 1.22;
	    answer[1][18][0] = 254.3;
	    answer[1][18][1] = 19.50;
	    answer[1][18][2] = 8.53;
	    answer[1][18][3] = 5.63;
	    answer[1][18][4] = 4.36;
	    answer[1][18][5] = 3.67;
	    answer[1][18][6] = 3.23;
	    answer[1][18][7] = 2.93;
	    answer[1][18][8] = 2.71;
	    answer[1][18][9] = 2.54;
	    answer[1][18][10] = 2.40;
	    answer[1][18][11] = 2.30;
	    answer[1][18][12] = 2.21;
	    answer[1][18][13] = 2.13;
	    answer[1][18][14] = 2.07;
	    answer[1][18][15] = 2.01;
        answer[1][18][16] = 1.96;
	    answer[1][18][17] = 1.92;
	    answer[1][18][18] = 1.88;
	    answer[1][18][19] = 1.84;
	    answer[1][18][20] = 1.81;
	    answer[1][18][21] = 1.78;
	    answer[1][18][22] = 1.76;
	    answer[1][18][23] = 1.73;
	    answer[1][18][24] = 1.71;
	    answer[1][18][25] = 1.69;
	    answer[1][18][26] = 1.67;
	    answer[1][18][27] = 1.65;
	    answer[1][18][28] = 1.64;
	    answer[1][18][29] = 1.62;
	    answer[1][18][30] = 1.51;
	    answer[1][18][31] = 1.39;
	    answer[1][18][32] = 1.25;
	    answer[1][18][33] = 1.00;
	    answer[2][0][0] = 4052;
		answer[2][0][1] = 98.50;
		answer[2][0][2] = 34.12;
		answer[2][0][3] = 21.20;
		answer[2][0][4] = 16.26;
		answer[2][0][5] = 13.75;
		answer[2][0][6] = 12.25;
		answer[2][0][7] = 11.26;
		answer[2][0][8] = 10.56;
		answer[2][0][9] = 10.04;
		answer[2][0][10] = 9.65;
		answer[2][0][11] = 9.33;
		answer[2][0][12] = 9.07;
		answer[2][0][13] = 8.86;
		answer[2][0][14] = 8.68;
		answer[2][0][15] = 8.53;
		answer[2][0][16] = 8.40;
		answer[2][0][17] = 8.29;
		answer[2][0][18] = 8.18;
		answer[2][0][19] = 8.10;
		answer[2][0][20] = 8.02;
		answer[2][0][21] = 7.95;
		answer[2][0][22] = 7.88;
		answer[2][0][23] = 7.82;
		answer[2][0][24] = 7.77;
		answer[2][0][25] = 7.72;
		answer[2][0][26] = 7.68;
		answer[2][0][27] = 7.64;
		answer[2][0][28] = 7.60;
		answer[2][0][29] = 7.56;
		answer[2][0][30] = 7.31;
		answer[2][0][31] = 7.08;
		answer[2][0][32] = 6.85;
		answer[2][0][33] = 6.63;
		answer[2][1][0] = 4999.5;
		answer[2][1][1] = 99.00;
		answer[2][1][2] = 30.82;
		answer[2][1][3] = 18.00;
		answer[2][1][4] = 13.27;
		answer[2][1][5] = 10.92;
		answer[2][1][6] = 9.55;
		answer[2][1][7] = 8.65;
		answer[2][1][8] = 8.02;
		answer[2][1][9] = 7.56;
		answer[2][1][10] = 7.21;
		answer[2][1][11] = 6.93;
		answer[2][1][12] = 6.70;
		answer[2][1][13] = 6.51;
		answer[2][1][14] = 6.36;
		answer[2][1][15] = 6.23;
		answer[2][1][16] = 6.11;
		answer[2][1][17] = 6.01;
		answer[2][1][18] = 5.93;
		answer[2][1][19] = 5.85;
		answer[2][1][20] = 5.78;
		answer[2][1][21] = 5.72;
		answer[2][1][22] = 5.66;
		answer[2][1][23] = 5.61;
		answer[2][1][24] = 5.57;
		answer[2][1][25] = 5.53;
		answer[2][1][26] = 5.49;
		answer[2][1][27] = 5.45;
		answer[2][1][28] = 5.42;
		answer[2][1][29] = 5.39;
		answer[2][1][30] = 5.18;
		answer[2][1][31] = 4.98;
		answer[2][1][32] = 4.79;
		answer[2][1][33] = 4.61;
		answer[2][2][0] = 5403;
		answer[2][2][1] = 99.17;
		answer[2][2][2] = 29.46;
		answer[2][2][3] = 16.69;
		answer[2][2][4] = 12.06;
		answer[2][2][5] = 9.78;
		answer[2][2][6] = 8.45;
		answer[2][2][7] = 7.59;
		answer[2][2][8] = 6.99;
		answer[2][2][9] = 6.55;
		answer[2][2][10] = 6.22;
		answer[2][2][11] = 5.95;
		answer[2][2][12] = 5.74;
		answer[2][2][13] = 5.56;
		answer[2][2][14] = 5.42;
		answer[2][2][15] = 5.29;
		answer[2][2][16] = 5.18;
		answer[2][2][17] = 5.09;
		answer[2][2][18] = 5.01;
		answer[2][2][19] = 4.94;
		answer[2][2][20] = 4.87;
		answer[2][2][21] = 4.82;
		answer[2][2][22] = 4.76;
		answer[2][2][23] = 4.72;
		answer[2][2][24] = 4.68;
		answer[2][2][25] = 4.64;
		answer[2][2][26] = 4.60;
		answer[2][2][27] = 4.57;
		answer[2][2][28] = 4.54;
		answer[2][2][29] = 4.51;
		answer[2][2][30] = 4.31;
		answer[2][2][31] = 4.13;
		answer[2][2][32] = 3.95;
		answer[2][2][33] = 3.78;
		answer[2][3][0] = 5625;
		answer[2][3][1] = 99.25;
		answer[2][3][2] = 28.71;
		answer[2][3][3] = 15.98; 
		answer[2][3][4] = 11.39;
		answer[2][3][5] = 9.15;
		answer[2][3][6] = 7.85;
		answer[2][3][7] = 7.01;
		answer[2][3][8] = 6.42;
		answer[2][3][9] = 5.99;
		answer[2][3][10] = 5.67;
		answer[2][3][11] = 5.41;
		answer[2][3][12] = 5.21;
		answer[2][3][13] = 5.04;
		answer[2][3][14] = 4.89;
		answer[2][3][15] = 4.77;
		answer[2][3][16] = 4.67;
		answer[2][3][17] = 4.58;
		answer[2][3][18] = 4.50;
		answer[2][3][19] = 4.43;
		answer[2][3][20] = 4.37;
		answer[2][3][21] = 4.31;
		answer[2][3][22] = 4.26;
		answer[2][3][23] = 4.22;
		answer[2][3][24] = 4.18;
		answer[2][3][25] = 4.14;
		answer[2][3][26] = 4.11;
		answer[2][3][27] = 4.07;
		answer[2][3][28] = 4.04;
		answer[2][3][29] = 4.02;
		answer[2][3][30] = 3.83;
		answer[2][3][31] = 3.65;
		answer[2][3][32] = 3.48;
		answer[2][3][33] = 3.32;
		answer[2][4][0] = 5764;
		answer[2][4][1] = 99.30;
		answer[2][4][2] = 28.24;
		answer[2][4][3] = 15.52;
		answer[2][4][4] = 10.97;
		answer[2][4][5] = 8.75;
		answer[2][4][6] = 7.46;
		answer[2][4][7] = 6.63;
		answer[2][4][8] = 6.06;
		answer[2][4][9] = 5.64;
		answer[2][4][10] = 5.32;
		answer[2][4][11] = 5.06;
		answer[2][4][12] = 4.86;
		answer[2][4][13] = 4.69;
		answer[2][4][14] = 4.56;
		answer[2][4][15] = 4.44;
		answer[2][4][16] = 4.34;
		answer[2][4][17] = 4.25;
		answer[2][4][18] = 4.17;
		answer[2][4][19] = 4.10;
		answer[2][4][20] = 4.04;
		answer[2][4][21] = 3.99;
		answer[2][4][22] = 3.94;
		answer[2][4][23] = 3.90;
		answer[2][4][24] = 3.85;
		answer[2][4][25] = 3.82;
		answer[2][4][26] = 3.78;
		answer[2][4][27] = 3.75;
		answer[2][4][28] = 3.73;
		answer[2][4][29] = 3.70;
	    answer[2][4][30] = 3.51;
	    answer[2][4][31] = 3.34;
	    answer[2][4][32] = 3.17;
	    answer[2][4][33] = 3.02;
	    answer[2][5][0] = 5859;
	    answer[2][5][1] = 99.33;
	    answer[2][5][2] = 27.91;
	    answer[2][5][3] = 15.21;
	    answer[2][5][4] = 10.67;
	    answer[2][5][5] = 8.47;
	    answer[2][5][6] = 7.19;
	    answer[2][5][7] = 6.37;
	    answer[2][5][8] = 5.80;
	    answer[2][5][9] = 5.39;
	    answer[2][5][10] = 5.07;
	    answer[2][5][11] = 4.82;
	    answer[2][5][12] = 4.62;
	    answer[2][5][13] = 4.46;
	    answer[2][5][14] = 4.32;
	    answer[2][5][15] = 4.20;
	    answer[2][5][16] = 4.10;
	    answer[2][5][17] = 4.01;
	    answer[2][5][18] = 3.94;
	    answer[2][5][19] = 3.87;
	    answer[2][5][20] = 3.81;
	    answer[2][5][21] = 3.76;
	    answer[2][5][22] = 3.71;
	    answer[2][5][23] = 3.67;
	    answer[2][5][24] = 3.63;
	    answer[2][5][25] = 3.59;
	    answer[2][5][26] = 3.56;
	    answer[2][5][27] = 3.53;
	    answer[2][5][28] = 3.50;
	    answer[2][5][29] = 3.47;
	    answer[2][5][30] = 3.29;
	    answer[2][5][31] = 3.12;
	    answer[2][5][32] = 2.96;
	    answer[2][5][33] = 2.80;
	    answer[2][6][0] = 5928;
	    answer[2][6][1] = 99.36;
	    answer[2][6][2] = 27.67;
	    answer[2][6][3] = 14.98;
	    answer[2][6][4] = 10.46;
	    answer[2][6][5] = 8.26;
	    answer[2][6][6] = 6.99;
	    answer[2][6][7] = 6.18;
	    answer[2][6][8] = 5.61;
	    answer[2][6][9] = 5.20;
	    answer[2][6][10] = 4.89;
	    answer[2][6][11] = 4.64;
	    answer[2][6][12] = 4.44;
	    answer[2][6][13] = 4.28;
	    answer[2][6][14] = 4.14;
	    answer[2][6][15] = 4.03;
	    answer[2][6][16] = 3.93;
	    answer[2][6][17] = 3.84;
	    answer[2][6][18] = 3.77;
	    answer[2][6][19] = 3.70;
	    answer[2][6][20] = 3.64;
	    answer[2][6][21] = 3.59;
	    answer[2][6][22] = 3.54;
	    answer[2][6][23] = 3.50;
	    answer[2][6][24] = 3.46;
	    answer[2][6][25] = 3.42;
	    answer[2][6][26] = 3.39;
	    answer[2][6][27] = 3.36;
	    answer[2][6][28] = 3.33;
	    answer[2][6][29] = 3.30;
	    answer[2][6][30] = 3.12;
	    answer[2][6][31] = 2.95;
	    answer[2][6][32] = 2.79;
	    answer[2][6][33] = 2.64;
	    answer[2][7][0] = 5982;
	    answer[2][7][1] = 99.37;
	    answer[2][7][2] = 27.49;
	    answer[2][7][3] = 14.80;
	    answer[2][7][4] = 10.29;
	    answer[2][7][5] = 8.10;
	    answer[2][7][6] = 6.84;
	    answer[2][7][7] = 6.03;
	    answer[2][7][8] = 5.47;
	    answer[2][7][9] = 5.06;
	    answer[2][7][10] = 4.74;
	    answer[2][7][11] = 4.50;
	    answer[2][7][12] = 4.30;
	    answer[2][7][13] = 4.14;
	    answer[2][7][14] = 4.00;
	    answer[2][7][15] = 3.89;
	    answer[2][7][16] = 3.79;
	    answer[2][7][17] = 3.71;
	    answer[2][7][18] = 3.63;
	    answer[2][7][19] = 3.56;
	    answer[2][7][20] = 3.51;
	    answer[2][7][21] = 3.45;
	    answer[2][7][22] = 3.41;
	    answer[2][7][23] = 3.36;
	    answer[2][7][24] = 3.32;
	    answer[2][7][25] = 3.29;
	    answer[2][7][26] = 3.26;
	    answer[2][7][27] = 3.23;
	    answer[2][7][28] = 3.20;
	    answer[2][7][29] = 3.17;
	    answer[2][7][30] = 2.99;
	    answer[2][7][31] = 2.82;
	    answer[2][7][32] = 2.66;
	    answer[2][7][33] = 2.51;
	    answer[2][8][0] = 6022;
	    answer[2][8][1] = 99.39;
	    answer[2][8][2] = 27.35;
	    answer[2][8][3] = 14.66;
	    answer[2][8][4] = 10.16;
	    answer[2][8][5] = 7.98;
	    answer[2][8][6] = 6.72;
	    answer[2][8][7] = 5.91;
	    answer[2][8][8] = 5.35;
	    answer[2][8][9] = 4.94;
	    answer[2][8][10] = 4.63;
	    answer[2][8][11] = 4.39;
	    answer[2][8][12] = 4.19;
	    answer[2][8][13] = 4.03;
	    answer[2][8][14] = 3.89;
	    answer[2][8][15] = 3.78;
	    answer[2][8][16] = 3.68;
	    answer[2][8][17] = 3.60;
	    answer[2][8][18] = 3.52;
	    answer[2][8][19] = 3.46;
	    answer[2][8][20] = 3.40;
	    answer[2][8][21] = 3.35;
	    answer[2][8][22] = 3.30;
	    answer[2][8][23] = 3.26;
	    answer[2][8][24] = 3.22;
	    answer[2][8][25] = 3.18;
	    answer[2][8][26] = 3.15;
	    answer[2][8][27] = 3.12;
	    answer[2][8][28] = 3.09;
	    answer[2][8][29] = 3.07;
	    answer[2][8][30] = 2.89;
	    answer[2][8][31] = 2.72;
	    answer[2][8][32] = 2.56;
	    answer[2][8][33] = 2.41;
	    answer[2][9][0] = 6056;
	    answer[2][9][1] = 99.40;
	    answer[2][9][2] = 27.23;
	    answer[2][9][3] = 14.55;
	    answer[2][9][4] = 10.05;
	    answer[2][9][5] = 7.87;
	    answer[2][9][6] = 6.62;
	    answer[2][9][7] = 5.81;
	    answer[2][9][8] = 5.26;
	    answer[2][9][9] = 4.85;
	    answer[2][9][10] = 4.54;
	    answer[2][9][11] = 4.30;
	    answer[2][9][12] = 4.10;
	    answer[2][9][13] = 3.94;
	    answer[2][9][14] = 3.80;
	    answer[2][9][15] = 3.69;
	    answer[2][9][16] = 3.59;
	    answer[2][9][17] = 3.51;
	    answer[2][9][18] = 3.43;
	    answer[2][9][19] = 3.37;
	    answer[2][9][20] = 3.31;
	    answer[2][9][21] = 3.26;
	    answer[2][9][22] = 3.21;
	    answer[2][9][23] = 3.17;
	    answer[2][9][24] = 3.13;
	    answer[2][9][25] = 3.09;
	    answer[2][9][26] = 3.06;
	    answer[2][9][27] = 3.03;
	    answer[2][9][28] = 3.00;
	    answer[2][9][29] = 2.98;
	    answer[2][9][30] = 2.80;
	    answer[2][9][31] = 2.63;
	    answer[2][9][32] = 2.47;
	    answer[2][9][33] = 2.32;
	    answer[2][10][0] = 6106;
	    answer[2][10][1] = 99.42;
	    answer[2][10][2] = 27.05;
	    answer[2][10][3] = 14.37;
	    answer[2][10][4] = 9.89;
	    answer[2][10][5] = 7.72;
	    answer[2][10][6] = 6.47;
	    answer[2][10][7] = 5.67;
	    answer[2][10][8] = 5.11;
	    answer[2][10][9] = 4.71;
	    answer[2][10][10] = 4.40;
	    answer[2][10][11] = 4.16;
	    answer[2][10][12] = 3.96;
	    answer[2][10][13] = 3.80;
	    answer[2][10][14] = 3.67;
	    answer[2][10][15] = 3.55;
	    answer[2][10][16] = 3.46;
	    answer[2][10][17] = 3.37;
	    answer[2][10][18] = 3.30;
	    answer[2][10][19] = 3.23;
	    answer[2][10][20] = 3.17;
	    answer[2][10][21] = 3.12;
	    answer[2][10][22] = 3.07;
	    answer[2][10][23] = 3.03;
	    answer[2][10][24] = 2.99;
	    answer[2][10][25] = 2.96;
	    answer[2][10][26] = 2.93;
	    answer[2][10][27] = 2.90;
	    answer[2][10][28] = 2.87;
	    answer[2][10][29] = 2.84;
	    answer[2][10][30] = 2.66;
	    answer[2][10][31] = 2.50;
	    answer[2][10][32] = 2.34;
	    answer[2][10][33] = 2.18;
	    /*answer[2][11][0] = 245.9;
	    answer[2][11][1] = 19.43;
	    answer[2][11][2] = 8.70;
	    answer[2][11][3] = 5.86;
	    answer[2][11][4] = 4.62;
	    answer[2][11][5] = 3.94;
	    answer[2][11][6] = 3.51;
	    answer[2][11][7] = 3.22;
	    answer[2][11][8] = 3.01;
	    answer[2][11][9] = 2.85;
	    answer[2][11][10] = 2.72;
	    answer[2][11][11] = 2.62;
	    answer[2][11][12] = 2.53;
	    answer[2][11][13] = 2.46;
	    answer[2][11][14] = 2.40;
	    answer[2][11][15] = 2.35;
        answer[2][11][16] = 2.31;
	    answer[2][11][17] = 2.27;
	    answer[2][11][18] = 2.23;
	    answer[2][11][19] = 2.20;
	    answer[2][11][20] = 2.18;
	    answer[2][11][21] = 2.15;
	    answer[2][11][22] = 2.13;
	    answer[2][11][23] = 2.11;
	    answer[2][11][24] = 2.09;
	    answer[2][11][25] = 2.07;
	    answer[2][11][26] = 2.06;
	    answer[2][11][27] = 2.04;
	    answer[2][11][28] = 2.03;
	    answer[2][11][29] = 2.01;
	    answer[2][11][30] = 1.92;
	    answer[2][11][31] = 1.84;
	    answer[2][11][32] = 1.75;
	    answer[2][11][33] = 1.67;
	    answer[2][12][0] = 248.0;
	    answer[2][12][1] = 19.45;
	    answer[2][12][2] = 8.66;
	    answer[2][12][3] = 5.80;
	    answer[2][12][4] = 4.56;
	    answer[2][12][5] = 3.87;
	    answer[2][12][6] = 3.44;
	    answer[2][12][7] = 3.15;
	    answer[2][12][8] = 2.94;
	    answer[2][12][9] = 2.77;
	    answer[2][12][10] = 2.65;
	    answer[2][12][11] = 2.54;
	    answer[2][12][12] = 2.46;
	    answer[2][12][13] = 2.39;
	    answer[2][12][14] = 2.33;
	    answer[2][12][15] = 2.28;
        answer[2][12][16] = 2.23;
	    answer[2][12][17] = 2.19;
	    answer[2][12][18] = 2.16;
	    answer[2][12][19] = 2.12;
	    answer[2][12][20] = 2.10;
	    answer[2][12][21] = 2.07;
	    answer[2][12][22] = 2.05;
	    answer[2][12][23] = 2.03;
	    answer[2][12][24] = 2.01;
	    answer[2][12][25] = 1.99;
	    answer[2][12][26] = 1.97;
	    answer[2][12][27] = 1.96;
	    answer[2][12][28] = 1.94;
	    answer[2][12][29] = 1.93;
	    answer[2][12][30] = 1.84;
	    answer[2][12][31] = 1.75;
	    answer[2][12][32] = 1.66;
	    answer[2][12][33] = 1.57;
	    answer[2][13][0] = 249.1;
	    answer[2][13][1] = 19.45;
	    answer[2][13][2] = 8.64;
	    answer[2][13][3] = 5.77;
	    answer[2][13][4] = 4.53;
	    answer[2][13][5] = 3.84;
	    answer[2][13][6] = 3.41;
	    answer[2][13][7] = 3.12;
	    answer[2][13][8] = 2.90;
	    answer[2][13][9] = 2.74;
	    answer[2][13][10] = 2.61;
	    answer[2][13][11] = 2.51;
	    answer[2][13][12] = 2.42;
	    answer[2][13][13] = 2.35;
	    answer[2][13][14] = 2.29;
	    answer[2][13][15] = 2.24;
        answer[2][13][16] = 2.19;
	    answer[2][13][17] = 2.15;
	    answer[2][13][18] = 2.11;
	    answer[2][13][19] = 2.08;
	    answer[2][13][20] = 2.05;
	    answer[2][13][21] = 2.03;
	    answer[2][13][22] = 2.01;
	    answer[2][13][23] = 1.98;
	    answer[2][13][24] = 1.96;
	    answer[2][13][25] = 1.95;
	    answer[2][13][26] = 1.93;
	    answer[2][13][27] = 1.91;
	    answer[2][13][28] = 1.90;
	    answer[2][13][29] = 1.89;
	    answer[2][13][30] = 1.79;
	    answer[2][13][31] = 1.70;
	    answer[2][13][32] = 1.61;
	    answer[2][13][33] = 1.52;
	    answer[2][14][0] = 250.1;
	    answer[2][14][1] = 19.46;
	    answer[2][14][2] = 8.62;
	    answer[2][14][3] = 5.75;
	    answer[2][14][4] = 4.50;
	    answer[2][14][5] = 3.81;
	    answer[2][14][6] = 3.38;
	    answer[2][14][7] = 3.08;
	    answer[2][14][8] = 2.86;
	    answer[2][14][9] = 2.70;
	    answer[2][14][10] = 2.57;
	    answer[2][14][11] = 2.47;
	    answer[2][14][12] = 2.38;
	    answer[2][14][13] = 2.31;
	    answer[2][14][14] = 2.25;
	    answer[2][14][15] = 2.19;
        answer[2][14][16] = 2.15;
	    answer[2][14][17] = 2.11;
	    answer[2][14][18] = 2.07;
	    answer[2][14][19] = 2.04;
	    answer[2][14][20] = 2.01;
	    answer[2][14][21] = 1.98;
	    answer[2][14][22] = 1.96;
	    answer[2][14][23] = 1.94;
	    answer[2][14][24] = 1.92;
	    answer[2][14][25] = 1.90;
	    answer[2][14][26] = 1.88;
	    answer[2][14][27] = 1.87;
	    answer[2][14][28] = 1.85;
	    answer[2][14][29] = 1.84;
	    answer[2][14][30] = 1.74;
	    answer[2][14][31] = 1.65;
	    answer[2][14][32] = 1.55;
	    answer[2][14][33] = 1.46;
	    answer[2][15][0] = 251.1;
	    answer[2][15][1] = 19.47;
	    answer[2][15][2] = 8.59;
	    answer[2][15][3] = 5.72;
	    answer[2][15][4] = 4.46;
	    answer[2][15][5] = 3.77;
	    answer[2][15][6] = 3.34;
	    answer[2][15][7] = 3.04;
	    answer[2][15][8] = 2.83;
	    answer[2][15][9] = 2.66;
	    answer[2][15][10] = 2.53;
	    answer[2][15][11] = 2.43;
	    answer[2][15][12] = 2.34;
	    answer[2][15][13] = 2.27;
	    answer[2][15][14] = 2.20;
	    answer[2][15][15] = 2.15;
        answer[2][15][16] = 2.10;
	    answer[2][15][17] = 2.06;
	    answer[2][15][18] = 2.03;
	    answer[2][15][19] = 1.99;
	    answer[2][15][20] = 1.96;
	    answer[2][15][21] = 1.94;
	    answer[2][15][22] = 1.91;
	    answer[2][15][23] = 1.89;
	    answer[2][15][24] = 1.87;
	    answer[2][15][25] = 1.85;
	    answer[2][15][26] = 1.84;
	    answer[2][15][27] = 1.82;
	    answer[2][15][28] = 1.81;
	    answer[2][15][29] = 1.79;
	    answer[2][15][30] = 1.69;
	    answer[2][15][31] = 1.59;
	    answer[2][15][32] = 1.50;
	    answer[2][15][33] = 1.39;
	    answer[2][16][0] = 252.2;
	    answer[2][16][1] = 19.48;
	    answer[2][16][2] = 8.57;
	    answer[2][16][3] = 5.69;
	    answer[2][16][4] = 4.43;
	    answer[2][16][5] = 3.74;
	    answer[2][16][6] = 3.30;
	    answer[2][16][7] = 3.01;
	    answer[2][16][8] = 2.79;
	    answer[2][16][9] = 2.62;
	    answer[2][16][10] = 2.49;
	    answer[2][16][11] = 2.38;
	    answer[2][16][12] = 2.30;
	    answer[2][16][13] = 2.22;
	    answer[2][16][14] = 2.16;
	    answer[2][16][15] = 2.11;
        answer[2][16][16] = 2.06;
	    answer[2][16][17] = 2.02;
	    answer[2][16][18] = 1.98;
	    answer[2][16][19] = 1.95;
	    answer[2][16][20] = 1.92;
	    answer[2][16][21] = 1.89;
	    answer[2][16][22] = 1.86;
	    answer[2][16][23] = 1.84;
	    answer[2][16][24] = 1.82;
	    answer[2][16][25] = 1.80;
	    answer[2][16][26] = 1.79;
	    answer[2][16][27] = 1.77;
	    answer[2][16][28] = 1.75;
	    answer[2][16][29] = 1.74;
	    answer[2][16][30] = 1.64;
	    answer[2][16][31] = 1.53;
	    answer[2][16][32] = 1.43;
	    answer[2][16][33] = 1.32;
	    answer[2][17][0] = 253.3;
	    answer[2][17][1] = 19.49;
	    answer[2][17][2] = 8.55;
	    answer[2][17][3] = 5.66;
	    answer[2][17][4] = 4.40;
	    answer[2][17][5] = 3.70;
	    answer[2][17][6] = 3.27;
	    answer[2][17][7] = 2.97;
	    answer[2][17][8] = 2.75;
	    answer[2][17][9] = 2.58;
	    answer[2][17][10] = 2.45;
	    answer[2][17][11] = 2.34;
	    answer[2][17][12] = 2.25;
	    answer[2][17][13] = 2.18;
	    answer[2][17][14] = 2.11;
	    answer[2][17][15] = 2.06;
        answer[2][17][16] = 2.01;
	    answer[2][17][17] = 1.97;
	    answer[2][17][18] = 1.93;
	    answer[2][17][19] = 1.90;
	    answer[2][17][20] = 1.87;
	    answer[2][17][21] = 1.84;
	    answer[2][17][22] = 1.81;
	    answer[2][17][23] = 1.79;
	    answer[2][17][24] = 1.77;
	    answer[2][17][25] = 1.75;
	    answer[2][17][26] = 1.73;
	    answer[2][17][27] = 1.71;
	    answer[2][17][28] = 1.70;
	    answer[2][17][29] = 1.68;
	    answer[2][17][30] = 1.58;
	    answer[2][17][31] = 1.47;
	    answer[2][17][32] = 1.35;
	    answer[2][17][33] = 1.22;
	    answer[2][18][0] = 254.3;
	    answer[2][18][1] = 19.50;
	    answer[2][18][2] = 8.53;
	    answer[2][18][3] = 5.63;
	    answer[2][18][4] = 4.36;
	    answer[2][18][5] = 3.67;
	    answer[2][18][6] = 3.23;
	    answer[2][18][7] = 2.93;
	    answer[2][18][8] = 2.71;
	    answer[2][18][9] = 2.54;
	    answer[2][18][10] = 2.40;
	    answer[2][18][11] = 2.30;
	    answer[2][18][12] = 2.21;
	    answer[2][18][13] = 2.13;
	    answer[2][18][14] = 2.07;
	    answer[2][18][15] = 2.01;
        answer[2][18][16] = 1.96;
	    answer[2][18][17] = 1.92;
	    answer[2][18][18] = 1.88;
	    answer[2][18][19] = 1.84;
	    answer[2][18][20] = 1.81;
	    answer[2][18][21] = 1.78;
	    answer[2][18][22] = 1.76;
	    answer[2][18][23] = 1.73;
	    answer[2][18][24] = 1.71;
	    answer[2][18][25] = 1.69;
	    answer[2][18][26] = 1.67;
	    answer[2][18][27] = 1.65;
	    answer[2][18][28] = 1.64;
	    answer[2][18][29] = 1.62;
	    answer[2][18][30] = 1.51;
	    answer[2][18][31] = 1.39;
	    answer[2][18][32] = 1.25;
	    answer[2][18][33] = 1.00;*/
    	int errors = 0;
    	int tests = 0;
		for (i = 0; i < 2; i++) {
			pi[0] = p[i];	
			qi[0] = q[i];		
			for (j = 0; j < 19; j++) {
				dfni[0] = dfn[j];
				for (k = 0; k < 34; k++) {
				    dfdi[0] = dfd[k];
				    cdff(2,pi,qi,f,dfni,dfdi,status,bound);
				    Preferences.debug("p = " + p[i] + " dfn = " + dfn[j] + " dfd = " + dfd[k] +
				    		" result = " + f[0] + " answer = " + answer[i][j][k] + "\n", Preferences.DEBUG_ALGORITHM);
				    tests++;
				    if (answer[i][j][k] >= 1000.0) {
				    	if (Math.round(f[0]-answer[i][j][k]) != 0.0) {
					    	Preferences.debug("Error detected\n", Preferences.DEBUG_ALGORITHM);
					    	errors++;
					    }	
				    }
				    else if (answer[i][j][k] >= 100.0) {
				    	if (Math.round((f[0]-answer[i][j][k])*10.0) != 0.0) {
					    	Preferences.debug("Error detected\n", Preferences.DEBUG_ALGORITHM);
					    	errors++;
					    }	
				    }
				    else {
					    if (Math.round((f[0]-answer[i][j][k])*100.0) != 0.0){
					    	Preferences.debug("Error detected\n", Preferences.DEBUG_ALGORITHM);
					    	errors++;
					    }
				    }
				}
			}
		}
		Preferences.debug("F distribution errors = " + errors + " in " + tests + " tests\n", Preferences.DEBUG_ALGORITHM);
		System.out.println("F distribution errors = " + errors + " in " + tests + " tests");
	}
	
    private void cdff(int which,double p[],double q[],double f[],double dfn[],double dfd[],int status[], double bound[]) {
	/**********************************************************************
	C
	C      SUBROUTINE CDFF( WHICH, P, Q, F, DFN, DFD, STATUS, BOUND )
	C               Cumulative Distribution Function
	C               F distribution
	C
	C
	C                              Function
	C
	C
	C     Calculates any one parameter of the F distribution
	C     given values for the others.
	C
	C
	C                              Arguments
	C
	C
	C     WHICH --> Integer indicating which of the next four argument
	C               values is to be calculated from the others.
	C               Legal range: 1..4
	C               iwhich = 1 : Calculate P and Q from F,DFN and DFD
	C               iwhich = 2 : Calculate F from P,Q,DFN and DFD
	C               iwhich = 3 : Calculate DFN from P,Q,F and DFD
	C               iwhich = 4 : Calculate DFD from P,Q,F and DFN
	C                    INTEGER WHICH
	C
	C       P <--> The integral from 0 to F of the f-density.
	C              Input range: [0,1].
	C                    DOUBLE PRECISION P
	C
	C       Q <--> 1-P.
	C              Input range: (0, 1].
	C              P + Q = 1.0.
	C                    DOUBLE PRECISION Q
	C
	C       F <--> Upper limit of integration of the f-density.
	C              Input range: [0, +infinity).
	C              Search range: [0,1E100]
	C                    DOUBLE PRECISION F
	C
	C     DFN < --> Degrees of freedom of the numerator sum of squares.
	C               Input range: (0, +infinity).
	C               Search range: [ 1E-100, 1E100]
	C                    DOUBLE PRECISION DFN
	C
	C     DFD < --> Degrees of freedom of the denominator sum of squares.
	C               Input range: (0, +infinity).
	C               Search range: [ 1E-100, 1E100]
	C                    DOUBLE PRECISION DFD
	C
	C     STATUS <-- 0 if calculation completed correctly
	C               -I if input parameter number I is out of range
	C                1 if answer appears to be lower than lowest
	C                  search bound
	C                2 if answer appears to be higher than greatest
	C                  search bound
	C                3 if P + Q .ne. 1
	C                    INTEGER STATUS
	C
	C     BOUND <-- Undefined if STATUS is 0
	C
	C               Bound exceeded by parameter number I if STATUS
	C               is negative.
	C
	C               Lower search bound if STATUS is 1.
	C
	C               Upper search bound if STATUS is 2.
	C
	C
	C                              Method
	C
	C
	C     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of
	C     Mathematical  Functions (1966) is used to reduce the computation
	C     of the  cumulative  distribution function for the  F  variate to
	C     that of an incomplete beta.
	C
	C     Computation of other parameters involve a seach for a value that
	C     produces  the desired  value  of P.   The search relies  on  the
	C     monotinicity of P with the other parameter.
	C
	C                              WARNING
	C
	C     The value of the  cumulative  F distribution is  not necessarily
	C     monotone in  either degrees of freedom.  There  thus may  be two
	C     values  that  provide a given CDF  value.   This routine assumes
	C     monotonicity and will find an arbitrary one of the two values.
	C
	C**********************************************************************
	C     .. Parameters ..
	      DOUBLE PRECISION tol
	      PARAMETER (tol=1.0D-8)
	      DOUBLE PRECISION atol
	      PARAMETER (atol=1.0D-50)
	      DOUBLE PRECISION zero,inf
	      PARAMETER (zero=1.0D-100,inf=1.0D100)*/
    	final double tol = 1.0E-8;
    	final double atol = 1.0E-50;
    	final double zero = 1.0E-100;
    	final double inf = 1.0E100;
	/*     ..
	C     .. Scalar Arguments ..
	      DOUBLE PRECISION bound,dfd,dfn,f,p,q
	      INTEGER status,which
	C     ..
	C     .. Local Scalars ..*/
	      double ccum[] = new double[1];
	      double cum[] = new double[1];
	      double fx[] = new double[1];
	      double pq;
	      boolean qhi[] = new boolean[1];
	      boolean qleft[]= new boolean[1];
	      boolean qporq = false;
	/*C     ..
	C     .. External Functions ..
	      DOUBLE PRECISION spmpar
	      EXTERNAL spmpar
	C     ..
	C     .. External Subroutines ..
	      EXTERNAL cumf,dinvr,dstinv
	C     ..
	C     .. Intrinsic Functions ..
	      INTRINSIC abs
	C     ..*/
	      boolean do10 = true;
	      boolean do20 = true;
	      boolean do30 = true;
	      boolean do40 = true;
	      boolean do50 = true;
	      boolean do70 = true;
	      boolean do80 = true;
	      boolean do90 = true;
	      boolean do110 = true;
	      boolean do130 = true;
	      boolean do150 = true;
	      boolean do170 = true;
	      boolean do180 = true;
	      boolean do190 = true;
	      if (! ((which < 1) || (which > 4))) {
	    	  do10 = false;
	    	  do20 = false;
	      }
	      else if (! (which < 1)) {
	    	  
	      }
	      else {
	          bound[0] = 1.0;
	          do10 = false;
	      }

	      if (do10) {
	       bound[0] = 4.0;
	      } // if (do10)
	   if (do20) {
	      status[0] = -1;
	      return;
	   } // if (do20)

	   if (do30) {
	      if (which == 1) {
	    	  do40 = false;
	    	  do50 = false;
	      }
	      else if(! ((p[0] < 0.0) || (p[0] > 1.0))) {
	    	  do40 = false;
	    	  do50 = false;
	      }
	      else if (! (p[0] < 0.0)) {
	    	 
	      }
	      else {
	          bound[0] = 0.0;
	          do40 = false;
	      }
	   } // if (do30)

	   if (do40) {
	       bound[0] = 1.0;
	   } // if (do40)
	   if (do50) {
	       status[0] = -2;
	       return;
	   } // if (do50)

	   
	   if (do70) {
	      if (which == 1) {
	    	  do80 = false;
	    	  do90 = false;
	      }
	      else if (! ((q[0] <= 0.0) || (q[0] > 1.0))) {
	    	  do80 = false;
	    	  do90 = false;
	      }
	      else if (! (q[0] <= 0.0)) {
	    	  
	      }
	      else {
	          bound[0] = 0.0;
	          do80 = false;
	      }
	   } // if (do70)

	   if (do80) {
	      bound[0] = 1.0;
	   } // if (do80)
	   if (do90) {
	      status[0] = -3;
	      return;
	   } // if (do90)

	  
	  if (do110) {
	      if (which == 2) {
	      }
	      else if (! (f[0] < 0.0)) {
	    	  
	      }
	      else {
	          bound[0] = 0.0;
	          status[0] = -4;
	          return;
	      }
	  } // if (do110)

	 
	  if (do130) {
	      if (which == 3) {
	    	  
	      }
	      else if (! (dfn[0] <= 0.0)) {
	    	  
	      }
	      else {
	          bound[0] = 0.0;
	          status[0] = -5;
	          return;
	      }
	  }

	  if (do150) {
	      if (which == 4) {
	    	  
	      }
	      else if (! (dfd[0] <= 0.0)) {
	    	  
	      }
	      else {
	          bound[0] = 0.0;
	          status[0] = -6;
	          return;
	      }
	  } // if (do150)

	
	  if (do170) {
	      if (which == 1) {
	    	  do180 = false;
	    	  do190 = false;
	      }
	      else {
	          pq = p[0] + q[0];
	          if (! (Math.abs(((pq)-0.5)-0.5) >
	          (3.0*spmpar(1)))) {
	        	  do180 = false;
	        	  do190 = false;
	          }
	          else if (! (pq < 0.0)) {
	        	  
	          }
	          else {
	              bound[0] = 0.0;
	              do180 = false;
	          }
	      }
	  } // if (do170)

	  if (do180) {
	      bound[0] = 1.0;
	  } // if (do180
	  if (do190) {
		  status[0] = 3;
	      return;
	  } // if (do190)

	      if (! (which == 1)) qporq = p[0] <= q[0];
	      if (which == 1) {
	          cumf(f[0],dfn[0],dfd[0],p,q);
	          status[0] = 0;
	      }
	      else if (which == 2) {
	          f[0] = 5.0;
	          dstinv(0.0,inf,0.5,0.5,5.0,atol,tol);
	          status[0] = 0;
	          dinvr(status,f,fx,qleft,qhi);
	          while (true) {
	              if (! (status[0] == 1)) break;
	              cumf(f[0],dfn[0],dfd[0],cum,ccum);
	              if (qporq) {
	                  fx[0] = cum[0] - p[0];
	              }
	              else {
	                  fx[0] = ccum[0] - q[0];
	              }
	              dinvr(status,f,fx,qleft,qhi);
	          } // while (true)

	          if (status[0] == -1) {
		          if (qleft[0]) {
		              status[0] = 1;
		              bound[0] = 0.0;
		          }
		          else {
		              status[0] = 2;
		              bound[0] = inf;
		          }
	          } // if (status[0] == -1)
	      } // else if (which == 2)
	      else if (which == 3) {
	          dfn[0] = 5.0;
	          dstinv(zero,inf,0.5,0.5,5.0,atol,tol);
	          status[0] = 0;
	          dinvr(status,dfn,fx,qleft,qhi);
	          while (true) {
		          if (! (status[0] == 1)) break;
		          cumf(f[0],dfn[0],dfd[0],cum,ccum);
		          if (qporq) {
		              fx[0] = cum[0] - p[0];
		          }
		          else {
		              fx[0] = ccum[0] - q[0];
		          }
		          dinvr(status,dfn,fx,qleft,qhi);
	          } // while (true)

	          if (status[0]== -1) {
		          if (qleft[0]) {
		              status[0] = 1;
		              bound[0] = zero;
		          }
		          else {
		              status[0] = 2;
		              bound[0] = inf;
		          }
	          } // if (status[0] == -1)
	      } // else if (which == 3)

	      else if (which == 4) {
	          dfd[0] = 5.0;
	          dstinv(zero,inf,0.5,0.5,5.0,atol,tol);
	          status[0] = 0;
	          dinvr(status,dfd,fx,qleft,qhi);
	          while (true) {
		          if (! (status[0] == 1)) break;
		          cumf(f[0],dfn[0],dfd[0],cum,ccum);
		          if (qporq) {
		              fx[0] = cum[0] - p[0];
		          }
		          else {
		              fx[0] = ccum[0] - q[0];
		          }
		          dinvr(status,dfd,fx,qleft,qhi);
	          } // while (true)

	          if (status[0] == -1) {
		          if (qleft[0]) {
		              status[0] = 1;
		              bound[0] = zero;
		          }
		          else {
		              status[0] = 2;
		              bound[0] = inf;
		          }
	          } // if (status[0] == -1)
	      } // else if (which == 4)

	      return;

    }
    
    private void dstinv(double zsmall,double zbig,double zabsst,double zrelst,
    		double zstpmu,double zabsto,double zrelto) {
    /**********************************************************************
    C
    C      SUBROUTINE DSTINV( SMALL, BIG, ABSSTP, RELSTP, STPMUL,
    C     +                   ABSTOL, RELTOL )
    C      Double Precision - SeT INverse finder - Reverse Communication
    C
    C
    C                              Function
    C
    C
    C     Concise Description - Given a monotone function F finds X
    C     such that F(X) = Y.  Uses Reverse communication -- see invr.
    C     This routine sets quantities needed by INVR.
    C
    C          More Precise Description of INVR -
    C
    C     F must be a monotone function, the results of QMFINV are
    C     otherwise undefined.  QINCR must be .TRUE. if F is non-
    C     decreasing and .FALSE. if F is non-increasing.
    C
    C     QMFINV will return .TRUE. if and only if F(SMALL) and
    C     F(BIG) bracket Y, i. e.,
    C          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or
    C          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL)
    C
    C     if QMFINV returns .TRUE., then the X returned satisfies
    C     the following condition.  let
    C               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
    C     then if QINCR is .TRUE.,
    C          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X))
    C     and if QINCR is .FALSE.
    C          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
    C
    C
    C                              Arguments
    C
    C
    C     SMALL --> The left endpoint of the interval to be
    C          searched for a solution.
    C                    SMALL is DOUBLE PRECISION
    C
    C     BIG --> The right endpoint of the interval to be
    C          searched for a solution.
    C                    BIG is DOUBLE PRECISION
    C
    C     ABSSTP, RELSTP --> The initial step size in the search
    C          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm.
    C                    ABSSTP is DOUBLE PRECISION
    C                    RELSTP is DOUBLE PRECISION
    C
    C     STPMUL --> When a step doesn't bound the zero, the step
    C                size is multiplied by STPMUL and another step
    C                taken.  A popular value is 2.0
    C                    DOUBLE PRECISION STPMUL
    C
    C     ABSTOL, RELTOL --> Two numbers that determine the accuracy
    C          of the solution.  See function for a precise definition.
    C                    ABSTOL is DOUBLE PRECISION
    C                    RELTOL is DOUBLE PRECISION
    C
    C
    C                              Method
    C
    C
    C     Compares F(X) with Y for the input value of X then uses QINCR
    C     to determine whether to step left or right to bound the
    C     desired x.  the initial step size is
    C          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X.
    C     Iteratively steps right or left until it bounds X.
    C     At each step which doesn't bound X, the step size is doubled.
    C     The routine is careful never to step beyond SMALL or BIG.  If
    C     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE.
    C     after setting QLEFT and QHI.
    C
    C     If X is successfully bounded then Algorithm R of the paper
    C     'Two Efficient Algorithms with Guaranteed Convergence for
    C     Finding a Zero of a Function' by J. C. P. Bus and
    C     T. J. Dekker in ACM Transactions on Mathematical
    C     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed
    C     to find the zero of the function F(X)-Y. This is routine
    C     QRZERO.
    C
    C***********************************************************************/
          small = zsmall;
          big = zbig;
          absstp = zabsst;
          relstp = zrelst;
          stpmul = zstpmu;
          abstol = zabsto;
          reltol = zrelto;
          return;
    }

    private void dinvr(int status[],double x[],double fx[], boolean qleft[], boolean qhi[]) {
    /**********************************************************************
    C
    C     SUBROUTINE DINVR(STATUS, X, FX, QLEFT, QHI)
    C          Double precision
    C          bounds the zero of the function and invokes zror
    C                    Reverse Communication
    C
    C
    C                              Function
    C
    C
    C     Bounds the    function  and  invokes  ZROR   to perform the   zero
    C     finding.  STINVR  must  have   been  called  before this   routine
    C     in order to set its parameters.
    C
    C
    C                              Arguments
    C
    C
    C     STATUS <--> At the beginning of a zero finding problem, STATUS
    C                 should be set to 0 and INVR invoked.  (The value
    C                 of parameters other than X will be ignored on this cal
    C
    C                 When INVR needs the function evaluated, it will set
    C                 STATUS to 1 and return.  The value of the function
    C                 should be set in FX and INVR again called without
    C                 changing any of its other parameters.
    C
    C                 When INVR has finished without error, it will return
    C                 with STATUS 0.  In that case X is approximately a root
    C                 of F(X).
    C
    C                 If INVR cannot bound the function, it returns status
    C                 -1 and sets QLEFT and QHI.
    C                         INTEGER STATUS
    C
    C     X <-- The value of X at which F(X) is to be evaluated.
    C                         DOUBLE PRECISION X
    C
    C     FX --> The value of F(X) calculated when INVR returns with
    C            STATUS = 1.
    C                         DOUBLE PRECISION FX
    C
    C     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that
    C          case it is .TRUE. If the stepping search terminated
    C          unsucessfully at SMALL.  If it is .FALSE. the search
    C          terminated unsucessfully at BIG.
    C                    QLEFT is LOGICAL
    C
    C     QHI <-- Defined only if QMFINV returns .FALSE.  In that
    C          case it is .TRUE. if F(X) .GT. Y at the termination
    C          of the search and .FALSE. if F(X) .LT. Y at the
    C          termination of the search.
    C                    QHI is LOGICAL

    C
    C**********************************************************************
    C     .. Scalar Arguments ..
          DOUBLE PRECISION fx,x,zabsst,zabsto,zbig,zrelst,zrelto,zsmall,
         +                 zstpmu
          INTEGER status
          LOGICAL qhi,qleft
    C     ..
        Replace local variables with globals to replace effect of SAVE statement.
         ..
    C     .. External Subroutines ..
          EXTERNAL dstzr,dzror
    C     ..
    C     .. Intrinsic Functions ..
          INTRINSIC abs,max,min
    C     ..
    C     .. Statement Functions ..
          LOGICAL qxmon
    C     ..
    C     .. Save statement ..
          SAVE
    C     ..
    C     .. Statement Function definitions ..
          qxmon(zx,zy,zz) = zx .LE. zy .AND. zy .LE. zz
    C     ..
    C     .. Executable Statements ..*/
         boolean do5 = true;
         boolean do10 = true;
         boolean do20 = true;
         boolean do30 = true;
         boolean do50 = true;
         boolean do60 = true;
         boolean do80 = true;
         boolean do90 = true;
         boolean do100 = true;
         boolean do110 = true;
         boolean do120 = true;
         boolean do130 = true;
         boolean do140 = true;
         boolean do150 = true;
         boolean do170 = true;
         boolean do180 = true;
         boolean do190 = true;
         boolean do200 = true;
         boolean do210 = true;
         boolean do220 = true;
         boolean do240 = true;
         boolean do250 = true;
         

          if (status[0] > 0) {
              if (i99999_dinvr >= 10) {
            	  do5 = false;
              }
              if (i99999_dinvr >= 20) {
            	  do10 = false;
              }
              if (i99999_dinvr >= 30) {
            	  do20 = false;
              }
              if (i99999_dinvr >= 40) {
            	  do30 = false;
              }
              if (i99999_dinvr >= 60) {
            	  do50 = false;
              }
              if (i99999_dinvr >= 80) {
            	  do60 = false;
              }
              if (i99999_dinvr >= 90) {
            	  do80 = false;
              }
              if (i99999_dinvr >= 100) {
            	  do90 = false;
              }
              if (i99999_dinvr >= 110) {
            	  do100 = false;
              }
              if (i99999_dinvr >= 120) {
            	  do110 = false;
              }
              if (i99999_dinvr >= 130) {
            	  do120 = false;
              }
              if (i99999_dinvr >= 140) {
            	  do130 = false;
              }
              if (i99999_dinvr >= 150) {
            	  do140 = false;
              }
              if (i99999_dinvr >= 170) {
            	  do150 = false;
              }
              if (i99999_dinvr >= 180) {
            	  do170 = false;
              }
              if (i99999_dinvr >= 190) {
            	  do180 = false;
              }
              if (i99999_dinvr >= 200) {
            	  do190 = false;
              }
              if (i99999_dinvr >= 210) {
            	  do200 = false;
              }
              if (i99999_dinvr >= 220) {
            	  do210 = false;
              }
              if (i99999_dinvr >= 240) {
            	  do220 = false;
              }
              if (i99999_dinvr >= 250) {
            	  do240 = false;
              }
          } // if (status[0] > 0)

          if (do5) {
	          qcond_dinvr = !qxmon(small,x[0],big);
	          if (qcond_dinvr) {
	        	  MipavUtil.displayError("SMALL, X[0], BIG not monotone in INVR");
	        	  return;
	          }
	          xsave_dinvr = x[0];
	    
	    //    See that SMALL and BIG bound the zero and set QINCR
	    
	          x[0] = small;
	    //     GET-FUNCTION-VALUE
	          i99999_dinvr = 10;
	          status[0] = 1;
	          return;
          } // if (do5)

       if (do10) {
          fsmall_dinvr = fx[0];
          x[0] = big;
    //     GET-FUNCTION-VALUE
          i99999_dinvr = 20;
          status[0] = 1;
          return;
       } // if (do10)

       if (do20) {
          fbig_dinvr = fx[0];
          qincr_dinvr = fbig_dinvr > fsmall_dinvr;
          if (! (qincr_dinvr)) {
        	  do30 = false;
          }
          else if (fsmall_dinvr <= 0.0) {
        	  
          }
          else {
              status[0] = -1;
              qleft[0] = true;
              qhi[0] = true;
              return;
          }
       } // if (do20)

       if (do30) {
          if (fbig_dinvr >= 0.0) {
        	  do50 = false;
        	  do60 = false;
          }
          else {
	          status[0] = -1;
	          qleft[0] = false;
	          qhi[0] = false;
	          return;
          }
       } // if (do30)

       if (do50) {
          if (fsmall_dinvr >= 0.0) {
        	  
          }
          else {
	          status[0] = -1;
	          qleft[0] = true;
	          qhi[0] = false;
	          return;
          }
       } // if (do50)

       if (do60) {
          if (fbig_dinvr <= 0.0) {
        	  
          }
          else {
	          status[0] = -1;
	          qleft[0] = false;
	          qhi[0] = true;
	          return;
	          
          }
       } // if (do60)

       if (do80) {
          x[0] = xsave_dinvr;
          step_dinvr = Math.max(absstp,relstp*Math.abs(x[0]));
    //      YY = F(X) - Y
    //     GET-FUNCTION-VALUE
          i99999_dinvr = 90;
          status[0] = 1;
          return;
       } // if (do80)

       if (do90) {
          yy_dinvr = fx[0];
          if (! (yy_dinvr == 0.0)) {
        	
          }
          else {
	          status[0] = 0;
	          //qok = true;
	          return;
          }
       } // if (do90)

      if (do100) {
           qup_dinvr = (qincr_dinvr && (yy_dinvr < 0.0)) ||
               (!qincr_dinvr && (yy_dinvr > 0.0));
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    //     HANDLE CASE IN WHICH WE MUST STEP HIGHER
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          if (! (qup_dinvr)) {
        	  do110 = false;
        	  do120 = false;
        	  do130 = false;
        	  do140 = false;
        	  do150 = false;
          }
          else {
              xlb_dinvr = xsave_dinvr;
              xub_dinvr = Math.min(xlb_dinvr+step_dinvr,big);
              do110= false;
          }
      } // if (do100)

      if (do110) {
          if (qcond_dinvr) {
        	  do120 = false;
        	  do130 = false;
        	  do140 = false;
          }
    //      YY = F(XUB) - Y
      } // if (do110_dinvr)
      if (do120) {
          x[0] = xub_dinvr;
    //    GET-FUNCTION-VALUE
          i99999_dinvr = 130;
          status[0] = 1;
          return;
      } // if (do120)

      if (do130) {
          yy_dinvr = fx[0];
          qbdd_dinvr = (qincr_dinvr && (yy_dinvr >= 0.0)) ||
               (!qincr_dinvr && (yy_dinvr <= 0.0));
          qlim_dinvr = xub_dinvr >= big;
          qcond_dinvr = qbdd_dinvr || qlim_dinvr;
          if (qcond_dinvr) {
        	  
          }
          else {
	          step_dinvr = stpmul*step_dinvr;
	          xlb_dinvr = xub_dinvr;
	          xub_dinvr = Math.min(xlb_dinvr+step_dinvr,big);
          }
      } // if (do130)
      if (do140) {
       if (qcond_dinvr) {
    	   
       }
       else {
    	   x[0] = xub_dinvr;
    	    //    GET-FUNCTION-VALUE
          i99999_dinvr = 130;
          status[0] = 1;
          return;   
       }
      } // if (do140)

      if (do150) {
          if (! (qlim_dinvr && !qbdd_dinvr)) {
        	  do170 = false;
        	  do180 = false;
        	  do190 = false;
        	  do200 = false;
        	  do210 = false;
        	  do220 = false;
          }
          else {
	          status[0] = -1;
	          qleft[0] = false;
	          qhi[0] =  !qincr_dinvr;
	          x[0] = big;
	          return;
          }
      } // if (do150)

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    //     HANDLE CASE IN WHICH WE MUST STEP LOWER
    
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if (do170) {
          xub_dinvr = xsave_dinvr;
          xlb_dinvr = Math.max(xub_dinvr-step_dinvr,small);
          do180 = false;
      } // if (do170)

      if (do180) {
          if (qcond_dinvr) {
        	  do190 = false;
        	  do200 = false;
        	  do210 = false;
          }
    //      YY = F(XLB) - Y
      } // if (do180)
      if (do190) {
          x[0] = xlb_dinvr;
    //     GET-FUNCTION-VALUE
          i99999_dinvr = 200;
          status[0] = 1;
          return;
      } // if (do190)

      if (do200) {
          yy_dinvr = fx[0];
          qbdd_dinvr = (qincr_dinvr && (yy_dinvr <= 0.0)) ||
                (!qincr_dinvr && (yy_dinvr >= 0.0));
          qlim_dinvr = xlb_dinvr <= small;
          qcond_dinvr = qbdd_dinvr || qlim_dinvr;
          if (qcond_dinvr) {
        	  do210 = false;
          }
          else {
	          step_dinvr = stpmul*step_dinvr;
	          xub_dinvr = xlb_dinvr;
	          xlb_dinvr = Math.max(xub_dinvr-step_dinvr,small);
          }
      } // if (do200)
      if (do210) {
    	  x[0] = xlb_dinvr;
    	    //     GET-FUNCTION-VALUE
          i99999_dinvr = 200;
          status[0] = 1;
          return;
      } // if (do210)

      if (do220) {
          if (! (qlim_dinvr && !qbdd_dinvr)) {
        	  
          }
          else {
	          status[0] = -1;
	          qleft[0] = true;
	          qhi[0] = qincr_dinvr;
	          x[0] = small;
	          return;
          }
      } // if (do220)

      if (do240) {
          dstzr(xlb_dinvr,xub_dinvr,abstol,reltol);
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    //     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F.
    
   //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          status[0] = 0;
          do250 = false;
      } // if (do240)

      if (do250) {
          if (! (status[0] == 1)) {
        	  x[0] = xlo_dinvr[0];
        	  status[0] = 0;
        	  return;
          }
      } // if (do250)
      dzror(status,x,fx,xlo_dinvr,xhi_dinvr,qdum1_dinvr,qdum2_dinvr);
      if (! (status[0] == 1)) {
    	  x[0] = xlo_dinvr[0];
    	  status[0] = 0;
    	  return;
      }
      // GET-FUNCTION-VALUE
      i99999_dinvr = 250;
      status[0] = 1;
      return;

    }
    
    private void dstzr(double zxlo, double zxhi, double zabstl, double zreltl) {
    /**********************************************************************
    C
    C     SUBROUTINE DSTZR( XLO, XHI, ABSTOL, RELTOL )
    C     Double precision SeT ZeRo finder - Reverse communication version
    C
    C
    C                              Function
    C
    C
    C
    C     Sets quantities needed by ZROR.  The function of ZROR
    C     and the quantities set is given here.
    C
    C     Concise Description - Given a function F
    C     find XLO such that F(XLO) = 0.
    C
    C          More Precise Description -
    C
    C     Input condition. F is a double precision function of a single
    C     double precision argument and XLO and XHI are such that
    C          F(XLO)*F(XHI)  .LE.  0.0
    C
    C     If the input condition is met, QRZERO returns .TRUE.
    C     and output values of XLO and XHI satisfy the following
    C          F(XLO)*F(XHI)  .LE. 0.
    C          ABS(F(XLO)  .LE. ABS(F(XHI)
    C          ABS(XLO-XHI)  .LE. TOL(X)
    C     where
    C          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
    C
    C     If this algorithm does not find XLO and XHI satisfying
    C     these conditions then QRZERO returns .FALSE.  This
    C     implies that the input condition was not met.
    C
    C
    C                              Arguments
    C
    C
    C     XLO --> The left endpoint of the interval to be
    C           searched for a solution.
    C                    XLO is DOUBLE PRECISION
    C
    C     XHI --> The right endpoint of the interval to be
    C           for a solution.
    C                    XHI is DOUBLE PRECISION
    C
    C     ABSTOL, RELTOL --> Two numbers that determine the accuracy
    C                      of the solution.  See function for a
    C                      precise definition.
    C                    ABSTOL is DOUBLE PRECISION
    C                    RELTOL is DOUBLE PRECISION
    C
    C
    C                              Method
    C
    C
    C     Algorithm R of the paper 'Two Efficient Algorithms with
    C     Guaranteed Convergence for Finding a Zero of a Function'
    C     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
    C     Mathematical Software, Volume 1, no. 4 page 330
    C     (Dec. '75) is employed to find the zero of F(X)-Y.
    C
    C**********************************************************************/
          xxlo = zxlo;
          xxhi = zxhi;
          abstol = zabstl;
          reltol = zreltl;
          return;
    }
    
    private void dzror(int status[],double x[], double fx[], double xlo[], double xhi[], boolean qleft[], boolean qhi[]) {
    /**********************************************************************
    C
    C     SUBROUTINE DZROR(STATUS, X, FX, XLO, XHI, QLEFT, QHI)
    C     Double precision ZeRo of a function -- Reverse Communication
    C
    C
    C                              Function
    C
    C
    C     Performs the zero finding.  STZROR must have been called before
    C     this routine in order to set its parameters.
    C
    C
    C                              Arguments
    C
    C
    C     STATUS <--> At the beginning of a zero finding problem, STATUS
    C                 should be set to 0 and ZROR invoked.  (The value
    C                 of other parameters will be ignored on this call.)
    C
    C                 When ZROR needs the function evaluated, it will set
    C                 STATUS to 1 and return.  The value of the function
    C                 should be set in FX and ZROR again called without
    C                 changing any of its other parameters.
    C
    C                 When ZROR has finished without error, it will return
    C                 with STATUS 0.  In that case (XLO,XHI) bound the answe
    C
    C                 If ZROR finds an error (which implies that F(XLO)-Y an
    C                 F(XHI)-Y have the same sign, it returns STATUS -1.  In
    C                 this case, XLO and XHI are undefined.
    C                         INTEGER STATUS
    C
    C     X <-- The value of X at which F(X) is to be evaluated.
    C                         DOUBLE PRECISION X
    C
    C     FX --> The value of F(X) calculated when ZROR returns with
    C            STATUS = 1.
    C                         DOUBLE PRECISION FX
    C
    C     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the
    C             inverval in X containing the solution below.
    C                         DOUBLE PRECISION XLO
    C
    C     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the
    C             inverval in X containing the solution above.
    C                         DOUBLE PRECISION XHI
    C
    C     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully
    C                at XLO.  If it is .FALSE. the search terminated
    C                unsucessfully at XHI.
    C                    QLEFT is LOGICAL
    C
    C     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the
    C              search and .FALSE. if F(X) .LT. Y at the
    C              termination of the search.
    C                    QHI is LOGICAL
    C
    C**********************************************************************
    C     .. Scalar Arguments ..
          DOUBLE PRECISION fx,x,xhi,xlo,zabstl,zreltl,zxhi,zxlo
          INTEGER status
          LOGICAL qhi,qleft
    C     ..
    C     .. Save statement ..
          SAVE
    C     ..
    C    Replaced local variables with globals to reflect SAVE statement.
    C     .. Intrinsic Functions ..
          INTRINSIC abs,max,sign
    C     ..
    C     .. Statement Functions ..
          DOUBLE PRECISION ftol
    C     ..
    C     .. Statement Function definitions ..
          ftol(zx) = 0.5D0*max(abstol,reltol*abs(zx))
    C     ..
    C     .. Executable Statements ..*/
          boolean do5 = true;
          boolean do10 = true;
          boolean do20 = true;
          boolean do40 = true;
          boolean do60 = true;
          boolean do70 = true;
          boolean do80 = true;
          boolean do90 = true;
          boolean do100 = true;
          boolean do110 = true;
          boolean do120 = true;
          boolean do130 = true;
          boolean do140 = true;
          boolean do150 = true;
          boolean do160 = true;
          boolean do190 = true;

          if (status[0] > 0) {
        	  if (i99999_dzror > 5) {
        		  do5 = false;
        	  }
        	  if (i99999_dzror > 10) {
        		  do10 = false;
        	  }
        	  if (i99999_dzror > 20) {
        		  do20 = false;
        	  }
        	  if (i99999_dzror > 40) {
        		  do40 = false;
        	  }
        	  if (i99999_dzror > 60) {
        		  do60 = false;
        	  }
        	  if (i99999_dzror > 70) {
        		  do70 = false;
        	  }
        	  if (i99999_dzror > 80) {
        		  do80 = false;
        	  }
        	  if (i99999_dzror > 90) {
        		  do90 = false;
        	  }
        	  if (i99999_dzror > 100) {
        		  do100 = false;
        	  }
        	  if (i99999_dzror > 110) {
        		  do110 = false;
        	  }
        	  if (i99999_dzror > 120) {
        		  do120 = false;
        	  }
        	  if (i99999_dzror > 130) {
        		  do130 = false;
        	  }
        	  if (i99999_dzror > 140) {
        		  do140 = false;
        	  }
        	  if (i99999_dzror > 150) {
        		  do150 = false;
        	  }
        	  if (i99999_dzror > 160) {
        		  do160 = false;
        	  }
        	  if (i99999_dzror > 190) {
        		  do190 = false;
        	  }
          } // if (status[0] > 0)
          if (do5) {
	          xlo[0] = xxlo;
	          xhi[0] = xxhi;
	          b_dzror = xlo[0];
	          x[0] = xlo[0];
	    //    GET-FUNCTION-VALUE
	          i99999_dzror = 10;
	          status[0] = 1;
	          return;
          } // if (do5)

       if (do10) {
          fb_dzror = fx[0];
          xlo[0] = xhi[0];
          a_dzror = xlo[0];
          x[0] = xlo[0];
    //    GET-FUNCTION-VALUE
          i99999_dzror = 20;
          status[0] = 1;
          return;
       } // if (do10)
    
    //     Check that F(ZXLO) < 0 < F(ZXHI)  or
    //                F(ZXLO) > 0 > F(ZXHI)
    
       if (do20) {
          if (! (fb_dzror < 0.0)) {
        	  
          }
          else if (! (fx[0] < 0.0)) {
        	  
          }
          else {
	          status[0] = -1;
	          qleft[0] = fx[0]< fb_dzror;
	          qhi[0] = false;
	          return;
          }
       } // if (do200

     if (do40) {
          if (! (fb_dzror > 0.0)) {
        	  
          }
          else if (! (fx[0] > 0.0)) {
        	  
          }
          else {
	          status[0] = -1;
	          qleft[0] = fx[0] > fb_dzror;
	          qhi[0] = true;
	          return;
          }
     } // if (do40)

      if (do60) {
          fa_dzror = fx[0];
          first_dzror = true;
      } // if (do60)
      while (true) {
      if (do70) {
          c_dzror = a_dzror;
          fc_dzror = fa_dzror;
          ext_dzror = 0;
      } // if (do70)
      if (do80) {
          if (! (Math.abs(fc_dzror) < Math.abs(fb_dzror))) {
        	  do90 = false;
          }
          else if (! (c_dzror != a_dzror)) {
        	  
          }
          else {
              d_dzror = a_dzror;
              fd_dzror = fa_dzror;
          }
      } // if (d080)
      if (do90) {
          a_dzror = b_dzror;
          fa_dzror = fb_dzror;
          xlo[0] = c_dzror;
          b_dzror = xlo[0];
          fb_dzror = fc_dzror;
          c_dzror = a_dzror;
          fc_dzror = fa_dzror;
      } // if (d090)
      if (do100) {
          tol_dzror = ftol(xlo[0]);
          m_dzror = (c_dzror+b_dzror)*.5;
          mb_dzror = m_dzror - b_dzror;
          if (! (Math.abs(mb_dzror) > tol_dzror)) {
        	  break;
          }
          else if (! (ext_dzror > 3)) {
          }
          else {
              w_dzror = mb_dzror;
              do110 = false;
              do120 = false;
              do130 = false;
              do140 = false;
              do150 = false;
              do160 = false;
          }
      } // if (do100)
      if (do110) {
    	  if (mb_dzror >= 0) {
    		  tol_dzror = Math.abs(tol_dzror);
    	  }
    	  else {
    		  tol_dzror = -Math.abs(tol_dzror);
    	  }
          p_dzror = (b_dzror-a_dzror)*fb_dzror;
          if (! (first_dzror)) {
        	  
          }
          else {
	          q_dzror = fa_dzror - fb_dzror;
	          first_dzror = false;
	          do120 = false;
          }
      } // if (do110)
      if (do120) {
          fdb_dzror = (fd_dzror-fb_dzror)/ (d_dzror-b_dzror);
          fda_dzror = (fd_dzror-fa_dzror)/ (d_dzror-a_dzror);
          p_dzror = fda_dzror*p_dzror;
          q_dzror = fdb_dzror*fa_dzror - fda_dzror*fb_dzror;
      } // if (do120)
      if (do130) {
          if (! (p_dzror < 0.0)) {
        	  
          }
          else {
              p_dzror = -p_dzror;
              q_dzror = -q_dzror;
          }
      } // if (do130)
      if (do140) {
          if (ext_dzror == 3) p_dzror = p_dzror*2.0;
          if (! (p_dzror == 0.0 || p_dzror <= (q_dzror*tol_dzror))) {
      
          }
          else {
              w_dzror = tol_dzror;
              do150 = false;
              do160 = false;
          }
      } // if (do140)
      if (do150) {
          if (! (p_dzror <  (mb_dzror*q_dzror))) {
   
          }
          else {
              w_dzror = p_dzror/q_dzror;
              do160 = false;
          }
      } // if (do150)

      if (do160) {
          w_dzror = mb_dzror;
      } // if (do160)
      if (do190) {
          d_dzror = a_dzror;
          fd_dzror = fa_dzror;
          a_dzror = b_dzror;
          fa_dzror = fb_dzror;
          b_dzror = b_dzror + w_dzror;
          xlo[0] = b_dzror;
          x[0] = xlo[0];
    //    GET-FUNCTION-VALUE
          i99999_dzror =  200;
          status[0] = 1;
          return;
      } // if (do190)

          fb_dzror = fx[0];
          if (! ((fc_dzror*fb_dzror) >= 0.0)) {
        	  
          }
          else {
        	  do70 = true;
        	  do80 = true;
        	  do90 = true;
        	  do100 = true;
        	  do110 = true;
        	  do120 = true;
        	  do130 = true;
        	  do140 = true;
        	  do150 = true;
        	  do160 = true;
        	  do190 = true;
              continue;  
          }
      

          if (! (w_dzror == mb_dzror)) {
        	 
          }
          else {
              ext_dzror = 0;
              do70 = false;
        	  do80 = true;
        	  do90 = true;
        	  do100 = true;
        	  do110 = true;
        	  do120 = true;
        	  do130 = true;
        	  do140 = true;
        	  do150 = true;
        	  do160 = true;
        	  do190 = true;
        	  continue;
          }

          ext_dzror = ext_dzror + 1;
          do70 = false;
    	  do80 = true;
    	  do90 = true;
    	  do100 = true;
    	  do110 = true;
    	  do120 = true;
    	  do130 = true;
    	  do140 = true;
    	  do150 = true;
    	  do160 = true;
    	  do190 = true;
      } // while (true)

          xhi[0] = c_dzror;
          qrzero_dzror = (fc_dzror >= 0.0 && fb_dzror <= 0.0) ||
                  (fc_dzror < 0.0 && fb_dzror >= 0.0);
          if (! (qrzero_dzror)) {
        	  status[0] = -1;
        	  return;
          }
          status[0] = 0;
          return;
    }

    private double ftol(double zx) {
    	double result = 0.5*Math.max(abstol,reltol*Math.abs(zx));
    	return result;
    }
    
    private boolean qxmon(double zx,double zy,double zz) {
    	boolean result;
    	result = (zx <= zy) && (zy <= zz);
    	return result;
    }



    
    private void cumf(double f,double dfn,double dfd,double cum[], double ccum[]) {
    /**********************************************************************
    C
    C     SUBROUTINE CUMF(F,DFN,DFD,CUM,CCUM)
    C                    CUMulative F distribution
    C
    C
    C                              Function
    C
    C
    C     Computes  the  integral from  0  to  F of  the f-density  with DFN
    C     and DFD degrees of freedom.
    C
    C
    C                              Arguments
    C
    C
    C     F --> Upper limit of integration of the f-density.
    C                                                  F is DOUBLE PRECISION
    C
    C     DFN --> Degrees of freedom of the numerator sum of squares.
    C                                                  DFN is DOUBLE PRECISI
    C
    C     DFD --> Degrees of freedom of the denominator sum of squares.
    C                                                  DFD is DOUBLE PRECISI
    C
    C     CUM <-- Cumulative f distribution.
    C                                                  CUM is DOUBLE PRECISI
    C
    C     CCUM <-- Compliment of Cumulative f distribution.
    C                                                  CCUM is DOUBLE PRECIS
    C
    C
    C                              Method
    C
    C
    C     Formula  26.5.28 of  Abramowitz and   Stegun   is  used to  reduce
    C     the cumulative F to a cumulative beta distribution.
    C
    C
    C                              Note
    C
    C
    C     If F is less than or equal to 0, 0 is returned.
    C
    C**********************************************************************
    C     .. Scalar Arguments ..
          DOUBLE PRECISION dfd,dfn,f,cum,ccum
    C     ..
    C     .. Local Scalars ..*/

          double dsum,prod,xx,yy;
          int ierr[] = new int[1];
    //     ..
    //     .. Parameters ..
    //      DOUBLE PRECISION half
          final double half = 0.5;
    //      DOUBLE PRECISION done
          final double done = 1.0;
    /*     ..
    C     .. External Subroutines ..
          EXTERNAL bratio
    C     ..
    C     .. Executable Statements ..*/

          if (f <= 0.0) {
              cum[0] = 0.0;
              ccum[0] = 1.0;
              return;
          }

          prod = dfn*f;
    /*
    C     XX is such that the incomplete beta with parameters
    C     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
    C
    C     YY is 1 - XX
    C
    C     Calculate the smaller of XX and YY accurately
    C*/
          dsum = dfd + prod;
          xx = dfd/dsum;
          if (xx > half) {
              yy = prod/dsum;
              xx = done - yy;
          }
          else {
              yy = done - xx;
          }

          bratio(dfd*half,dfn*half,xx,yy,ccum,cum,ierr);
          return;

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
    C     .. Local Scalars ..*/
          double b,binv,bm1,one,w,z, result;
          int emax,emin,ibeta,m;
    /*     ..
    C     .. External Functions ..
          INTEGER ipmpar
          EXTERNAL ipmpar
    C     ..
    C     .. Intrinsic Functions ..
          INTRINSIC dble
    C     ..
    C     .. Executable Statements ..
    */
          if (i == 1) {
	          b = ipmpar(4);
	          m = ipmpar(8);
	          result = Math.pow(b,(1-m));
	          return result;
          }
          else if (i == 2) {
	          b = ipmpar(4);
	          emin = ipmpar(9);
	          one = 1.0;
	          binv = one/b;
	          w = Math.pow(b,(emin+2));
	          result = ((w*binv)*binv)*binv;
	          return result;
          } // else if (i == 2)
          else {
	          ibeta = ipmpar(4);
	          m = ipmpar(8);
	          emax = ipmpar(10);
	    
	          b = ibeta;
	          bm1 = ibeta - 1;
	          one = 1.0;
	          z = Math.pow(b, (m-1));
	          w = ((z-one)*b+bm1)/ (b*z);
	    
	          z = Math.pow(b,(emax-2));
	          result = ((w*z)*b)*b;
	          return result;
          }

    }
    
    private int ipmpar(int i) {
    /*-----------------------------------------------------------------------
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
    C     .. Scalar Arguments ..
          INTEGER i
    C     ..
    C     .. Local Arrays ..
          INTEGER imach(10)
    C     ..
    C     .. Data statements ..*/
    	int IMACH[] = new int[11];
    	int result;
    
    //     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
    //     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
    //     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
    
          IMACH[ 1] =    2;
          IMACH[ 2] =    31;
          IMACH[ 3] = 2147483647;
          IMACH[ 4] =    2;
          IMACH[ 5] =    24;
          IMACH[ 6] =  -125;
          IMACH[ 7] =   128;
          IMACH[ 8] =    53;
          IMACH[ 9] = -1021;
          IMACH[10] =  1024;
    
          result = IMACH[i];
          return result;

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
    int n = 20;
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
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
    } // if (do50)

    if (do60) {
       w1[0] = apser(a0,b0,x0,eps);
       w[0] = 0.5 + (0.5-w1[0]);
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
    } // if (do60)

    if (do70) {
       w[0] = bpser(a0,b0,x0,eps);
       w1[0] = 0.5 + (0.5-w[0]);
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
    } // if (do70)
       

    if (do80) {
       w1[0] = bpser(b0,a0,y0,eps);
       w[0] = 0.5 + (0.5-w1[0]);
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
    } // if (do80)

    if (do90) {
       w[0] = bfrac(a0,b0,x0,y0,lambda,15.0*eps);

       w1[0] = 0.5 + (0.5-w[0]);
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
    } // if (do90)

    if (do100) {
       w1[0] = bup(b0,a0,y0,x0,n,eps);
       b0 = b0 + n;
    } // if (do100)

    if (do110) {
       bgrat(b0,a0,y0,x0,w1,15.0*eps,ierr1);

       w[0] = 0.5 + (0.5-w1[0]);
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
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
       else {
	       w[0] = w[0] + bpser(a0,b0,x0,eps);
	       w1[0] = 0.5 + (0.5-w[0]);
	       if (ind==0) return;
	       t = w[0];
	       w[0] = w1[0];
	       w1[0] = t;
	       return;
       }
    } // if (do120)

    if (do130) {
       if (a0<=15.0) {
         n = 20;
         w[0] = w[0] + bup(a0,b0,x0,y0,n,eps);
         a0 = a0 + n;
       }

       bgrat(a0,b0,x0,y0,w,15.0*eps,ierr1);

       w1[0] = 0.5 + (0.5-w[0]);
       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;
    } // if (do130)

    if (do140) {

       w[0] = basym(a0,b0,lambda,100.0*eps);

       w1[0] = 0.5 + (0.5-w[0]);
    } // if (do140)

    //  TERMINATION OF THE PROCEDURE



       if (ind==0) return;
       t = w[0];
       w[0] = w1[0];
       w1[0] = t;
       return;


     }
     
     private double basym(double a,double b,double lambda,double eps) {
     /*-----------------------------------------------------------------------
     !     ASYMPTOTIC EXPANSION FOR IX(A,B) FOR LARGE A AND B.
     !     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED.
     !     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT
     !     A AND B ARE GREATER THAN OR EQUAL TO 15.
     !-----------------------------------------------------------------------
     !------------------------
     !     ****** NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP
     !            ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN.
     !            THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1.
     !------------------------
     !     E0 = 2/SQRT(PI)
     !     E1 = 2**(-3/2)
     !------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: basym
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, eps, lambda
     ! ..
     ! .. Local Scalars ..*/
             double bsum, dsum, f, h, h2, hn, j0, j1, r, r0, r1, s,
               sum, t, t0, t1, u, w, w0, z, z0, z2, zn, znm1, result;
             int i, im1, imj, j, m, mm1, mmj, n, np1;
     
     // Local Arrays ..
             double a0[] = new double[21];
             double b0[] = new double[21];
             double c[] = new double[21];
             double d[] = new double[21];
     
     /* .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, SQRT
     ! ..
     ! .. Parameters ..*/
             final double e0 = 1.12837916709551;
             final double e1 = .353553390593274;
             final int num = 20;
     
             result = 0.0;

             if (a<b) {
               h = a/b;
               r0 = 1.0/(1.0+h);
               r1 = (b-a)/b;
               w0 = 1.0/Math.sqrt(a*(1.0+h));
             }
             else {
               h = b/a;
               r0 = 1.0/(1.0+h);
               r1 = (b-a)/a;
               w0 = 1.0/Math.sqrt(b*(1.0+h));
             }

             f = a*rlog1(-lambda/a) + b*rlog1(lambda/b);
             t = Math.exp(-f);

             if (t==0.0) return result;

             z0 = Math.sqrt(f);
             z = 0.5*(z0/e1);
             z2 = f + f;

             a0[0] = (2.0/3.0)*r1;
             c[0] = -0.5*a0[0];
             d[0] = -c[0];
             j0 = (0.5/e0)*erfc1(1,z0);
             j1 = e1;
             sum = j0 + d[0]*w0*j1;

             s = 1.0;
             h2 = h*h;
             hn = 1.0;
             w = w0;
             znm1 = z;
             zn = z2;
             for (n = 2; n <= num; n += 2) {
               hn = h2*hn;
               a0[n-1] = 2.0*r0*(1.0+h*hn)/(n+2.0);
               np1 = n + 1;
               s = s + hn;
               a0[np1-1] = 2.0*r1*s/(n+3.0);

               for (i = n; i <= np1; i++) {
                 r = -0.5*(i+1.0);
                 b0[0] = r*a0[0];
                 for (m = 2; m <= i; m++) {
                   bsum = 0.0;
                   mm1 = m - 1;
                   for (j = 1; j <= mm1; j++) {
                     mmj = m - j;
                     bsum = bsum + (j*r-mmj)*a0[j-1]*b0[mmj-1];
                   } // for (j = 1; j <= mm1; j++)
                   b0[m-1] = r*a0[m-1] + bsum/m;
                 } // for (m = 2; m <= i; m++)

                 c[i-1] = b0[i-1]/(i+1.0);

                 dsum = 0.0;
                 im1 = i - 1;
                 for (j = 1; j <= im1; j++) {
                   imj = i - j;
                   dsum = dsum + d[imj-1]*c[j-1];
                 } // for (j = 1; j <= im1; j++)
                 d[i-1] = -(dsum+c[i-1]);
               } // for (i = n; i <= np1; i++)

               j0 = e1*znm1 + (n-1.0)*j0;
               j1 = e1*zn + n*j1;
               znm1 = z2*znm1;
               zn = z2*zn;
               w = w0*w;
               t0 = d[n-1]*w*j0;
               w = w0*w;
               t1 = d[np1-1]*w*j1;
               sum = sum + (t0+t1);
               if (Math.abs(t0)+Math.abs(t1)<=eps*sum) break;
             } // for (n = 2; n <= num; n += 2)

             u = Math.exp(-bcorr(a,b));

             result = e0*t*u*sum;

             return result;

     }

     
     private void bgrat(double a,double b,double x,double y,double w[],double eps,int ierr[]) {
     /* .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, b, eps, x, y
             REAL (dpkind), INTENT (INOUT) :: w
             INTEGER, INTENT (OUT) :: ierr
     ! ..
     ! .. Local Scalars ..*/
             double bm1, bp2n, cn, coef, dj, j, l, lnx, n2, nu,
               r, s, sum, t, t2, u, v, z;
             double p[] = new double[1];
             double q[] = new double[1];
             int i, n, nm1;
    
     // .. Local Arrays ..
           double c[] = new double[30];
           double d[] = new double[30];
     /*
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, LOG
     ! ..
     !-----------------------------------------------------------------------
     !     ASYMPTOTIC EXPANSION FOR IX(A,B) WHEN A IS LARGER THAN B.
     !     THE RESULT OF THE EXPANSION IS ADDED TO W. IT IS ASSUMED
     !     THAT A .GE. 15 AND B .LE. 1.  EPS IS THE TOLERANCE USED.
     !     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
     !-----------------------------------------------------------------------*/
             bm1 = (b-0.5) - 0.5;
             nu = a + 0.5*bm1;

             if (y<=0.375) {
               lnx = alnrel(-y);
             }
             else {
               lnx = Math.log(x);
             }

             z = -nu*lnx;
             if (b*z==0.0) {
            	 // THE EXPANSION CANNOT BE COMPUTED

                 ierr[0] = 1;

                 return;
             }

     //                 COMPUTATION OF THE EXPANSION
     //                 SET R = EXP(-Z)*Z**B/GAMMA(B)

             r = b*(1.0+gam1(b))*Math.exp(b*Math.log(z));
             r = r*Math.exp(a*lnx)*Math.exp(0.5*bm1*lnx);
             u = algdiv(b,a) + b*Math.log(nu);
             u = r*Math.exp(-u);

             if (u!=0.0) {
               grat1(b,z,r,p,q,eps);

               v = 0.25*(1.0/nu)*(1.0/nu);
               t2 = 0.25*lnx*lnx;
               l = w[0]/u;
               j = q[0]/r;
               sum = j;
               t = 1.0;
               cn = 1.0;
               n2 = 0.0;
               for (n = 1; n <= 30; n++) {
                 bp2n = b + n2;
                 j = (bp2n*(bp2n+1.0)*j+(z+bp2n+1.0)*t)*v;
                 n2 = n2 + 2.0;
                 t = t*t2;
                 cn = cn/(n2*(n2+1.0));
                 c[n-1] = cn;
                 s = 0.0;

                 if (n!=1) {
                   nm1 = n - 1;
                   coef = b - n;
                   for (i = 1; i <= nm1; i++) {
                     s = s + coef*c[i-1]*d[n-i-1];
                     coef = coef + b;
                   } // for (i = 1; i <= nm1; i++)
                 } // if (n != 1)

                 d[n-1] = bm1*cn + s/n;
                 dj = d[n-1]*j;
                 sum = sum + dj;
                 if (sum<=0.0) {
                	 // THE EXPANSION CANNOT BE COMPUTED
                     ierr[0] = 1;
                     return;
                 }
                 if (Math.abs(dj)<=eps*(sum+l)) break;
               } // for (n = 1; n <= 30; n++)

     // ADD THE RESULTS TO W

               ierr[0] = 0;
               w[0] = w[0] + u*sum;
               return;

             } // if (u != 0.0)

     }
     
     private void grat1(double a,double x,double r,double p[],double q[],double eps) {
     /*-----------------------------------------------------------------------
     !        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
     !                      P(A,X) AND Q(A,X)
     !     IT IS ASSUMED THAT A .LE. 1.  EPS IS THE TOLERANCE TO BE USED.
     !     THE INPUT ARGUMENT R HAS THE VALUE E**(-X)*X**A/GAMMA(A).
     !-----------------------------------------------------------------------
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: a, eps, r, x
             REAL (dpkind), INTENT (OUT) :: p, q
     ! ..
     ! .. Local Scalars ..*/
             double a2n, a2nm1, am0, an0, b2n, b2nm1, cma,
               l, t, w;
             double an = 0.0;
             double c = 0.0;
             double sum = 0.0;
             double tol = 0.0;
             double g = 0.0;
             double j = 0.0;
             double z = 0.0;
             double h = 0.0;
             boolean do10 = true;
             boolean do20 = true;
             boolean do30 = true;
             boolean do40 = true;
             boolean do50 = true;
    
     /* Intrinsic Functions ..
             INTRINSIC ABS, EXP, LOG, SQRT
     */
             if (a*x==0.0) {
            	 if (x <= a) {
            		 p[0] = 0.0;
            	     q[0] = 1.0;
            	     return; 
            	 }
            	 p[0] = 1.0;
                 q[0] = 0.0;
                 return;
             }
             if (a==0.5) {
            	 if (x<0.25) {
                 p[0] = erf(Math.sqrt(x));
                 q[0] = 0.5 + (0.5-p[0]);
                 return;

            	 }

               q[0] = erfc1(0,Math.sqrt(x));
               p[0] = 0.5 + (0.5-q[0]);
               return;
             }
             if (x<1.1) {
            	 
             }
             else {
                 do10 = false;
                 do20 = false;
                 do30 = false;
                 do40 = false;
                 do50 = false;
             }

     //             TAYLOR SERIES FOR P(A,X)/X**A

     if (do10) {
             an = 3.0;
             c = x;
             sum = x/(a+3.0);
             tol = 0.1*eps/(a+1.0);
     } // if (do10)

     if (do20) {
        do {
             an = an + 1.0;
             c = -c*(x/an);
             t = c/(a+an);
             sum = sum + t;

        } while (Math.abs(t)>tol);

             j = a*x*((sum/6.0-0.5/(a+2.0))*x+1.0/(a+1.0));

             z = a*Math.log(x);
             h = gam1(a);
             g = 1.0 + h;

             if (x<0.25) {
            	 
             }
             else if (a<x/2.59) {
            	 do30 = false;
            	 do40 = false;
             }
             else {
            	 do30 = false;
             }
     } // if (do20)

     if (do30) {
             if (z>(-.13394)) {
            	 do40 = false;
             }
     } // if (do30)

     if (do40) {
             w = Math.exp(z);
             p[0] = w*g*(0.5+(0.5-j));
             q[0] = 0.5 + (0.5-p[0]);
             return;
     } // if (do40)

     if (do50) {
             l = rexp(z);
             w = 0.5 + (0.5+l);
             q[0] = (w*j-l)*g - h;
             if (q[0]<0.0) {
            	 p[0] = 1.0;
                 q[0] = 0.0;
                 return;
             }
             p[0] = 0.5 + (0.5-q[0]);
             return;
     } // if (do50)

     //              CONTINUED FRACTION EXPANSION

             a2nm1 = 1.0;
             a2n = 1.0;
             b2nm1 = x;
             b2n = x + (1.0-a);
             c = 1.0;

     do {
             a2nm1 = x*a2n + c*a2nm1;
             b2nm1 = x*b2n + c*b2nm1;
             am0 = a2nm1/b2nm1;
             c = c + 1.0;
             cma = c - a;
             a2n = a2nm1 + cma*a2n;
             b2n = b2nm1 + cma*b2n;
             an0 = a2n/b2n;

     } while(Math.abs(an0-am0)>=eps*an0);

             q[0] = r*an0;
             p[0] = 0.5 + (0.5-q[0]);
             return;

     }
     
     private double rexp(double x) {
     /*-----------------------------------------------------------------------
     !            EVALUATION OF THE FUNCTION EXP(X) - 1
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: rexp
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: x
     ! ..
     ! .. Local Scalars ..*/
             double w, result;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP
     ! ..
     ! .. Parameters ..*/
             final double p1 = .914041914819518E-09;
             final double p2 = .238082361044469E-01;
             final double q[] = new double[]{ 1.0, 
               -.499999999085958, .107141568980644, 
               -.119041179760821E-01, .595130811860248E-03};
     
             if (Math.abs(x)<=0.15) {
               result = x*(((p2*x+p1)*x+1.0)/evaluate_polynomial(q,x));
             }
             else {
               w = Math.exp(x);

               if (x<=0.0) {
                 result = (w-0.5) - 0.5;
               }
               else {
                 result = w*(0.5+(0.5-1.0/w));
               }
             }
             return result;
     }
     
     private double erf(double x) {
     /*-----------------------------------------------------------------------
     !             EVALUATION OF THE REAL ERROR FUNCTION
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: erf
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: x
     ! ..
     ! .. Local Scalars ..*/
             double ax, bot, t, top, x2, result;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP, SIGN
     ! ..
     ! .. Parameters ..*/
             final double a[] = new double[]{.128379167095513,
                .479137145607681E-01, .323076579225834E-01,
               -.133733772997339E-02, .771058495001320E-04};
             final double b[] = new double[]{ 1.0,
               .375795757275549, .538971687740286E-01,
               .301048631703895E-02};
             final double p[] = new double[]{ 3.00459261020162E+02,
               4.51918953711873E+02, 3.39320816734344E+02,
               1.52989285046940E+02, 4.31622272220567E+01,
               7.21175825088309, 5.64195517478974E-01,
               -1.36864857382717E-07};
             final double q[] = new double[]{ 3.00459260956983E+02,
               7.90950925327898E+02, 9.31354094850610E+02,
               6.38980264465631E+02, 2.77585444743988E+02,
               7.70001529352295E+01, 1.27827273196294E+01, 1.0};
             final double r[] = new double[]{2.82094791773523E-01,
               4.65807828718470, 2.13688200555087E+01,
               2.62370141675169E+01, 2.10144126479064};
             final double s[] = new double[]{1.0,
               1.80124575948747E+01, 9.90191814623914E+01,
               1.87114811799590E+02, 9.41537750555460E+01};
             final double c = .564189583547756;
     
             ax = Math.abs(x);

             if (ax<=0.5) {
               t = x*x;

               top = evaluate_polynomial(a,t) + 1.0;
               bot = evaluate_polynomial(b,t);

               result = x*(top/bot);

             }
             else if (ax<=4.0) {
               top = evaluate_polynomial(p,ax);
               bot = evaluate_polynomial(q,ax);

               result = 0.5 + (0.5-Math.exp(-x*x)*top/bot);

               if (x<0.0) result = -result;

             }
             else if (ax<5.8) {
               x2 = x*x;
               t = 1.0/x2;

               top = evaluate_polynomial(r,t);
               bot = evaluate_polynomial(s,t);

               result = (c-top/(x2*bot))/ax;
               result = 0.5 + (0.5-Math.exp(-x2)*result);

               if (x<0.0) result = -result;
             }
             else {
               if (x >= 0.0) {
            	   result = 1.0;
               }
               else {
            	   result = -1.0;
               }
             }
             return result;

     }
     
     private double erfc1(int ind,double x) {
     /*-----------------------------------------------------------------------
     !         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION
     !          ERFC1(IND,X) = ERFC(X)            IF IND = 0
     !          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: erfc1
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind), INTENT (IN) :: x
             INTEGER, INTENT (IN) :: ind
     ! ..
     ! .. Local Scalars ..*/
             double ax, bot, e, t, top, w;
             double result = 0.0;
             boolean do10 = true;
             boolean do20 = true;
             boolean do30 = true;
             boolean do40 = true;
     /*
     ! .. Intrinsic Functions ..
             INTRINSIC ABS, EXP
     ! ..
     ! .. Parameters ..*/
             final double a[] = new double[]{ .128379167095513,
               .479137145607681E-01, .323076579225834E-01,
               -.133733772997339E-02, .771058495001320E-04};
             final double b[] = new double[]{ 1.0,
               .375795757275549, .538971687740286E-01,
               .301048631703895E-02};
             final double p[] = new double[]{ 3.00459261020162E+02,
               4.51918953711873E+02, 3.39320816734344E+02,
               1.52989285046940E+02, 4.31622272220567E+01,
               7.21175825088309, 5.64195517478974E-01,
               -1.36864857382717E-07};
             final double q[] = new double[]{ 3.00459260956983E+02,
               7.90950925327898E+02, 9.31354094850610E+02,
               6.38980264465631E+02, 2.77585444743988E+02,
               7.70001529352295E+01, 1.27827273196294E+01, 1.0};
             final double r[] = new double[]{ 2.82094791773523E-01,
               4.65807828718470, 2.13688200555087E+01,
               2.62370141675169E+01, 2.10144126479064};
             final double s[] = new double[]{ 1.0,
               1.80124575948747E+01, 9.90191814623914E+01,
               1.87114811799590E+02, 9.41537750555460E+01};
             final double c = .564189583547756;
   
     //                     ABS(X) .LE. 0.5
             ax = Math.abs(x);

             if (ax<=0.5) {
     // x in [-0.5, 0.5]

               t = x*x;

               top = evaluate_polynomial(a,t) + 1.0;
               bot = evaluate_polynomial(b,t);

               result = 0.5 + (0.5-x*(top/bot));

               if (ind!=0) result = Math.exp(t)*result;

               return result;

             } // if (ax<=0.5)

             if (ax>4.0) {
            	 
             }
             else {
             top = evaluate_polynomial(p,ax);
             bot = evaluate_polynomial(q,ax);

             result = top/bot;
             do10 = false;
             do20 = false;
             }

     //                      ABS(X) .GT. 4

     if (do10) {

             if (x<=(-5.6)) {
            	 do20 = false;
            	 do30 = false;
             }

             else if (ind!=0) {
            
             }

             else if (x>100.0) {
            	 do20 = false;
            	 do30 = false;
            	 do40 = false;
             }

             else if (x*x>-exparg(1)) {
            	 do20 = false;
            	 do30 = false;
            	 do40 = false;
             }
     } // if (do10)

     if (do20) {

             t = (1.0/x);
             t = t * t;

             top = evaluate_polynomial(r,t);
             bot = evaluate_polynomial(s,t);

             result = (c-t*top/bot)/ax;
     } // if (do20)

     //                      FINAL ASSEMBLY

     if (do30) {

             if (ind!=0) {
               if (x<0.0) result = 2.0*Math.exp(x*x) - result;
               return result;

             }

             w = x*x;
             t = w;
             e = w - t;
             result = ((0.5+(0.5-e))*Math.exp(-t))*result;

             if (x<0.0) result = 2.0 - result;

             return result;
     } // if (do30)

     //             LIMIT VALUE FOR LARGE NEGATIVE X

     if (do40) {

             result = 2.0;

             if (ind!=0) result = 2.0*Math.exp(x*x);

             return result;
     } // if (do40)

     //   LIMIT VALUE FOR LARGE POSITIVE X (WHEN IND = 0)

             result = 0.0;
             return result;

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

             z = esum(mu,-(a*u+b*v));

             result = constant*Math.sqrt(b*x0)*z*Math.exp(-bcorr(a,b));
             return result;

    }
     
     private double esum(int mu, double x) {
     /*-----------------------------------------------------------------------
     !                    EVALUATION OF EXP(MU + X)
     !-----------------------------------------------------------------------
     ! .. Function Return Value ..
             REAL (dpkind) :: esum
     ! ..
     ! .. Scalar Arguments ..
             REAL (dpkind) :: x
             INTEGER :: mu
     ! ..
     ! .. Local Scalars ..*/
             double w;
             boolean do10 = true;
             double result;
     /* ..
     ! .. Intrinsic Functions ..
             INTRINSIC EXP
     */
             if (x>0.0) {
            	 
             }

             else if (mu<0) {
            	 do10 = false;
             }
             else {
                 w = mu + x;

                 if (w>0.0) {
                	 do10 = false;
                 }
                 else {
                     result = Math.exp(w);
                     return result;
                 }
             }

     if (do10) {
             if (mu>0) {
            	 
             }
             else {
	             w = mu + x;
	
	             if (w>=0.0) {
	               result = Math.exp(w);
	               return result;
	
	             }
             }
     }

             w = mu;
             result = Math.exp(w)*Math.exp(x);

             return result;

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
         -.648157123766197, -.448616543918019E+01,
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
         return (den*xmx0 + aug);

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

       return (aug + Math.log(x));



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
                return (gamln(a) + (gamln(b)-gamln(a+b)));
            }

            else {

                return (gamln(a) + algdiv(a,b));

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
    
}