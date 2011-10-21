/*
 *  Produced by f2java.  f2java is part of the Fortran-
 *  -to-Java project at the University of Tennessee Netlib
 *  numerical software repository.
 *
 *  Original authorship for the BLAS and LAPACK numerical
 *  routines may be found in the Fortran source, available at
 *  www.netlib.org.
 *
 *  Fortran input file: dcl1.f
 *
 *  The f2j compiler code was written by
 *  David M. Doolin (doolin@cs.utk.edu) and
 *  Keith  Seymour (seymour@cs.utk.edu)
 */
package gov.nih.mipav.model.algorithms.t2mapping.cj.math.dcl1;

import java.util.Arrays;
import java.lang.*;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;


public class Dcl1 {

// C THIS SUBROUTINE USES A MODIFICATION OF THE SIMPLEX 
// C METHOD OF LINEAR PROGRAMMING TO CALCULATE AN L1 SOLUTION 
// C TO A K BY N SYSTEM OF LINEAR EQUATIONS 
// C             AX=B 
// C SUBJECT TO L LINEAR EQUALITY CONSTRAINTS 
// C             CX=D 
// C AND M LINEAR INEQUALITY CONSTRAINTS 
// C             EX.LE.F. 
// C DESCRIPTION OF PARAMETERS 
// C K      NUMBER OF ROWS OF THE MATRIX A (K.GE.1). 
// C L      NUMBER OF ROWS OF THE MATRIX C (L.GE.0). 
// C M      NUMBER OF ROWS OF THE MATRIX E (M.GE.0). 
// C N      NUMBER OF COLUMNS OF THE MATRICES A,C,E (N.GE.1). 
// C KLMD   SET TO AT LEAST K+L+M FOR ADJUSTABLE DIMENSIONS. 
// C KLM2D  SET TO AT LEAST K+L+M+2 FOR ADJUSTABLE DIMENSIONS. 
// C NKLMD  SET TO AT LEAST N+K+L+M FOR ADJUSTABLE DIMENSIONS. 
// C N2D    SET TO AT LEAST N+2 FOR ADJUSTABLE DIMENSIONS 
// C Q      TWO DIMENSIONAL REAL ARRAY WITH KLM2D ROWS AND 
// C        AT LEAST N2D COLUMNS. 
// C        ON ENTRY THE MATRICES A,C AND E, AND THE VECTORS 
// C        B,D AND F MUST BE STORED IN THE FIRST K+L+M ROWS 
// C        AND N+1 COLUMNS OF Q AS FOLLOWS 
// C             A B 
// C         Q = C D 
// C             E F 
// C        THESE VALUES ARE DESTROYED BY THE SUBROUTINE. 
// C KODE   A CODE USED ON ENTRY TO, AND EXIT 
// C        FROM, THE SUBROUTINE. 
// C        ON ENTRY, THIS SHOULD NORMALLY BE SET TO 0. 
// C        HOWEVER, IF CERTAIN NONNEGATIVITY CONSTRAINTS 
// C        ARE TO BE INCLUDED IMPLICITLY, RATHER THAN 
// C        EXPLICITLY IN THE CONSTRAINTS EX.LE.F, THEN KODE 
// C        SHOULD BE SET TO 1, AND THE NONNEGATIVITY 
// C        CONSTRAINTS INCLUDED IN THE ARRAYS X AND 
// C        RES (SEE BELOW). 
// C        ON EXIT, KODE HAS ONE OF THE 
// C        FOLLOWING VALUES 
// C             0- OPTIMAL SOLUTION FOUND, 
// C             1- NO FEASIBLE SOLUTION TO THE 
// C                CONSTRAINTS, 
// C             2- CALCULATIONS TERMINATED 
// C                PREMATURELY DUE TO ROUNDING ERRORS, 
// C             3- MAXIMUM NUMBER OF ITERATIONS REACHED. 
// C TOLER  A SMALL POSITIVE TOLERANCE. EMPIRICAL 
// C        EVIDENCE SUGGESTS TOLER = 10**(-D*2/3), 
// C        WHERE D REPRESENTS THE NUMBER OF DECIMAL 
// C        DIGITS OF ACCURACY AVAILABLE. ESSENTIALLY, 
// C        THE SUBROUTINE CANNOT DISTINGUISH BETWEEN ZERO 
// C        AND ANY QUANTITY WHOSE MAGNITUDE DOES NOT EXCEED 
// C        TOLER. IN PARTICULAR, IT WILL NOT PIVOT ON ANY 
// C        NUMBER WHOSE MAGNITUDE DOES NOT EXCEED TOLER. 
// C ITER   ON ENTRY ITER MUST CONTAIN AN UPPER BOUND ON 
// C        THE MAXIMUM NUMBER OF ITERATIONS ALLOWED. 
// C        A SUGGESTED VALUE IS 10*(K+L+M). ON EXIT ITER 
// C        GIVES THE NUMBER OF SIMPLEX ITERATIONS. 
// C X      ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST N2D. 
// C        ON EXIT THIS ARRAY CONTAINS A 
// C        SOLUTION TO THE L1 PROBLEM. IF KODE=1 
// C        ON ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE 
// C        SIMPLE NONNEGATIVITY CONSTRAINTS ON THE 
// C        VARIABLES. THE VALUES -1, 0, OR 1 
// C        FOR X(J) INDICATE THAT THE J-TH VARIABLE 
// C        IS RESTRICTED TO BE .LE.0, UNRESTRICTED, 
// C        OR .GE.0 RESPECTIVELY. 
// C RES    ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST KLMD. 
// C        ON EXIT THIS CONTAINS THE RESIDUALS B-AX 
// C        IN THE FIRST K COMPONENTS, D-CX IN THE 
// C        NEXT L COMPONENTS (THESE WILL BE =0),AND 
// C        F-EX IN THE NEXT M COMPONENTS. IF KODE=1 ON 
// C        ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE SIMPLE 
// C        NONNEGATIVITY CONSTRAINTS ON THE RESIDUALS 
// C        B-AX. THE VALUES -1, 0, OR 1 FOR RES(I) 
// C        INDICATE THAT THE I-TH RESIDUAL (1.LE.I.LE.K) IS 
// C        RESTRICTED TO BE .LE.0, UNRESTRICTED, OR .GE.0 
// C        RESPECTIVELY. 
// C ERROR  ON EXIT, THIS GIVES THE MINIMUM SUM OF 
// C        ABSOLUTE VALUES OF THE RESIDUALS. 
// C CU     A TWO DIMENSIONAL REAL ARRAY WITH TWO ROWS AND 
// C        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE. 
// C IU     A TWO DIMENSIONAL INTEGER ARRAY WITH TWO ROWS AND 
// C        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE. 
// C S      INTEGER ARRAY OF SIZE AT LEAST KLMD, USED FOR 
// C        WORKSPACE. 
// C IF YOUR FORTRAN COMPILER PERMITS A SINGLE COLUMN OF A TWO 
// C DIMENSIONAL ARRAY TO BE PASSED TO A ONE DIMENSIONAL ARRAY 
// C THROUGH A SUBROUTINE CALL, CONSIDERABLE SAVINGS IN 
// C EXECUTION TIME MAY BE ACHIEVED THROUGH THE USE OF THE 
// C FOLLOWING SUBROUTINE, WHICH OPERATES ON COLUMN VECTORS. 
// C     SUBROUTINE COL(V1, V2, XMLT, NOTROW, K) 
// C THIS SUBROUTINE ADDS TO THE VECTOR V1 A MULTIPLE OF THE 
// C VECTOR V2 (ELEMENTS 1 THROUGH K EXCLUDING NOTROW). 
// C     DIMENSION V1(K), V2(K) 
// C     KEND = NOTROW - 1 
// C     KSTART = NOTROW + 1 
// C     IF (KEND .LT. 1) GO TO 20 
// C     DO 10 I=1,KEND 
// C        V1(I) = V1(I) + XMLT*V2(I) 
// C  10 CONTINUE 
// C     IF(KSTART .GT. K) GO TO 40 
// C  20 DO 30 I=KSTART,K 
// C       V1(I) = V1(I) + XMLT*V2(I) 
// C  30 CONTINUE 
// C  40 RETURN 
// C     END 
// C SEE COMMENTS FOLLOWING STATEMENT LABELLED 440 FOR 
// C INSTRUCTIONS ON THE IMPLEMENTATION OF THIS MODIFICATION. 
    double sum= 0.0;
    double dble= 0.0;
    double z= 0.0;
    double sn= 0.0;
    double zu= 0.0;
    double zv= 0.0;
    double cuv= 0.0;
    double xmax= 0.0;
    double xmin= 0.0;
    double pivot= 0.0;
    double tpivot= 0.0;
    double abs= 0.0;
    double dabs= 0.0;
    int i= 0;
    int j= 0;
    int ia= 0;
    int ii= 0;
    int in= 0;
    int js= 0;
    int kk= 0;
    int nk= 0;
    int n1= 0;
    int n2= 0;
    int jmn= 0;
    int jpn= 0;
    int klm= 0;
    int nkl= 0;
    int nk1= 0;
    int iimn= 0;
    int iout= 0;
    int klm1= 0;
    int klm2= 0;
    int nklm= 0;
    int nkl1= 0;
    int maxit= 0;
    int iphase= 0;
    int kforce= 0;
    int iabs= 0;

    long start=0L, end=0L;
// C 
// C      DBLE(X)=X 
// C      ABS(X)=DABS(X) 
// C INITIALIZATION. 
// C 

    int k;
    int l;
    int m;
    int n;
    int klmd;
    int klm2d;
    int nklmd;
    int n2d;
    double [] q = null;
    intW kode = null;
    double toler;
    intW iter = null;
    double [] x = null;
    double [] res = null;
    doubleW error = null;
    double [] cu = null;
    int [] iu = null;
    int [] s = null;

    public Dcl1()
    {
    }

    public void init (int k, int l, int m, int n, int klmd, int klm2d,
                      int nklmd, int n2d, intW kode, double toler, intW iter,
                      double [] x, double [] res, doubleW error, double [] cu,
                      int [] iu, int [] s)
    {
        this.k = k; this.l = l;  this.m = m; this.n = n;
        this.klmd = klmd; this.klm2d = klm2d; this.nklmd = nklmd;
        this.n2d = n2d;
        this.kode = new intW(kode.val);
        this.iter = new intW(iter.val);
        this.error = new doubleW(error.val);

        this.x = new double[ x.length ];
        System.arraycopy(x, 0, this.x, 0, x.length);
        this.res = new double[ res.length ];
        System.arraycopy(res, 0, this.res, 0, res.length);
        this.cu = new double[ cu.length ];
        System.arraycopy(cu, 0, this.cu, 0, cu.length);
        this.iu = new int[ iu.length ];
        System.arraycopy(iu, 0, this.iu, 0, iu.length);
        this.s = new int[ s.length ];
        System.arraycopy(s, 0, this.s, 0, s.length);
    }

    public void setQ( double [] q )
    {
        this.q = new double[ q.length ];
        System.arraycopy(q, 0, this.q, 0, q.length);
    }

    public void init()
    {
//	System.out.println("Init");
        maxit = iter.val;
        n1 = n+1;
        n2 = n+2;
        nk = n+k;
        nk1 = nk+1;
        nkl = nk+l;
        nkl1 = nkl+1;
        klm = k+l+m;
        klm1 = klm+1;
        klm2 = klm+2;
        nklm = n+klm;
        kforce = 1;
        iter.val = 0;
        js = 1;
        ia = 0;

        for (j = 0; j < n; j++)
        {
            q[(klm2)- 1+j*klm2d] = (double)(j+1);
        }

        for (i = 1; i <= klm; i++)
        {
            q[(i)- 1+(n2- 1)*klm2d] = (double)(n+i);
            if (q[(i)- 1+(n1- 1)*klm2d] >= 0.)
                continue;

            for (j = 1; j <= n2; j++) {
            q[(i)- 1+(j- 1)*klm2d] = -q[(i)- 1+(j- 1)*klm2d];
            }
        }
    }

    public void setup_phase1_costs()
    {

        //---------------------------------------------------
        //
        // C SET UP PHASE 1 COSTS.
        //
//		System.out.println("Setup phase 1 costs.");
        iphase = 2;

        //forloop40:
        //for (j = 1; j <= nklm; j++) {
        //cu[(j- 1)*2] = 0.;
        //cu[1+(j- 1)*2] = 0.;
        //iu[(j- 1)*2] = 0;
        //iu[1+(j- 1)*2] = 0;
        Arrays.fill(cu, 0.0);
        Arrays.fill(iu, 0);
        //}              //  Close for() loop.

        if (l != 0)
        {
            for (j = nk1; j <= nkl; j++) {
                cu[(j- 1)*2] = 1.;
                cu[1+(j- 1)*2] = 1.;
                iu[(j- 1)*2] = 1;
                iu[1+(j- 1)*2] = 1;
            }
            iphase = 1;
        }

        if (m != 0)
        {
            for (j = nkl1; j <= nklm; j++) {
                cu[1+(j- 1)*2] = 1.;
                iu[1+(j- 1)*2] = 1;
                jmn = j-n;
                if (q[(jmn)- 1+(n2- 1)*klm2d] < 0.)
                {
                    iphase = 1;
                }
            }
        }

        if (kode.val != 0)
        {

            for (j = 1; j <= n; j++)
            {
                if ((x[(j)- 1]) < 0)
                {
                    cu[(j- 1)*2] = 1.;
                    iu[(j- 1)*2] = 1;
                }
                else if ((x[(j)- 1]) > 0)
                {
                    cu[1+(j- 1)*2] = 1.;
                    iu[1+(j- 1)*2] = 1;
                }
            }


            for (j = 1; j <= k; j++)
            {
                jpn = j+n;
                if ((res[(j)- 1]) < 0)
                {
                    cu[(jpn- 1)*2] = 1.;
                    iu[(jpn- 1)*2] = 1;
                    if (q[(j)- 1+(n2- 1)*klm2d] > 0.0)
                        iphase = 1;
                }
                else if ((res[(j)- 1]) > 0)
                {
                    cu[1+(jpn- 1)*2] = 1.;
                    iu[1+(jpn- 1)*2] = 1;
                    if (q[(j)- 1+(n2- 1)*klm2d] < 0.0)
                        iphase = 1;
                }
            }
        }
    }

    public void dcl1 (double[] x)
    {

        init();

        setup_phase1_costs();

        if (iphase == 2)
            Dummy.go_to("Dcl1",500);

        //---------------------------------------------------
        //
        // C COMPUTE THE MARGINAL COSTS.
        //
        label160:
           Dummy.label("Dcl1",160);

//		 System.out.println("Compute the marginal costs.");

        for (j = js; j <= n1; j++)
        {
            sum = 0.e0;

            for (i = 1; i <= klm; i++)
            {
                ii = (int)(q[(i)- 1+(n2- 1)*klm2d]);

                if (ii < 0)
                {
                    z = cu[1+(-ii - 1)*2];
                }
                else
                {
                    z = cu[(ii- 1)*2];
                }

                sum = sum+(double)(q[(i)- 1+(j- 1)*klm2d])*(double)(z);
            }

            q[(klm1)- 1+(j- 1)*klm2d] = sum;
        }


        for (j = js; j <= n; j++)
        {
            ii = (int)(q[(klm2)- 1+(j- 1)*klm2d]);

            if (ii < 0)
            {
                z = cu[1+(-ii - 1)*2];
            }
            else
            {
                z = cu[(ii- 1)*2];
            }

            q[(klm1)- 1+(j- 1)*klm2d] = q[(klm1)- 1+(j- 1)*klm2d]-z;
        }


        //---------------------------------------------------
        //
        // C DETERMINE THE VECTOR TO ENTER THE BASIS.
        //
        label240:
           Dummy.label("Dcl1",240);

//		System.out.println("Determine vector to enter basis.");

        xmax = 0.;
        if (js > n)
            Dummy.go_to("Dcl1",490);

        for (j = js; j <= n; j++)
        {
            zu = q[(klm1)- 1+(j- 1)*klm2d];
            ii = (int)(q[(klm2)- 1+(j- 1)*klm2d]);
            if (ii > 0)
            {
                zv = -zu-cu[(ii- 1)*2]-cu[1+(ii- 1)*2];
            }
            else
            {
                ii = -ii;
                zv = zu;
                zu = -zu-cu[(ii- 1)*2]-cu[1+(ii- 1)*2];
            }

            if (kforce != 1 || ii <= n)
            {
                if ((iu[(ii- 1)*2] != 1) && zu > xmax)
                {
                    xmax = zu;
                    in = j;
                }

                if((iu[1+(ii- 1)*2] != 1) && (zv > xmax) )
                {
                    xmax = zv;
                    in = j;
                }
            }
        }

        if (xmax <= toler)
            Dummy.go_to("Dcl1",490);
        if (q[(klm1)- 1+(in- 1)*klm2d] == xmax)
            Dummy.go_to("Dcl1",300);

        for (i = 1; i <= klm2; i++) {
            q[(i)- 1+(in- 1)*klm2d] = -q[(i)- 1+(in- 1)*klm2d];
        }

        q[(klm1)- 1+(in- 1)*klm2d] = xmax;

        //--------------------------------------------------------------
        //
        // C DETERMINE THE VECTOR TO LEAVE THE BASIS.
        //
        label300:
           Dummy.label("Dcl1",300);

//		System.out.println("Determine vector to leave basis.");

        if (iphase != 1 && ia != 0)
        {
            xmax = 0.;

            for (i = 0; i < ia; i++)
            {
                z = (double)(Math.abs(q[i+(in- 1)*klm2d]));
                if (z > xmax)
                {
                    xmax = z;
                    iout = (i+1);
                }
            }

            if (xmax > toler)
            {
                for (j = 1; j <= n2; j++) {
                    z = q[(ia)- 1+(j- 1)*klm2d];
                    q[(ia)- 1+(j- 1)*klm2d] = q[(iout)- 1+(j- 1)*klm2d];
                    q[(iout)- 1+(j- 1)*klm2d] = z;
                }

                iout = ia;
                ia = ia-1;
                pivot = q[(iout)- 1+(in- 1)*klm2d];
                Dummy.go_to("Dcl1",420);
            }
        }

        kk = 0;

        for (i = 1; i <= klm; i++)
        {
            z = q[(i)- 1+(in- 1)*klm2d];
            if (z > toler)
            {
                kk++;
                res[(kk)- 1] = q[(i)- 1+(n1- 1)*klm2d]/z;
                s[(kk)- 1] = i;
            }
        }

        label350:
           Dummy.label("Dcl1",350);
        if (kk <= 0)
        {
            kode.val = 2;
            prepare_output(x);
            return;
        }

        xmin = res[(1)- 1];
        iout = s[(1)- 1];
        j = 1;

        if (kk != 1)
        {
            for (i = 2; i <= kk; i++)
            {
                if (res[(i)- 1] < xmin)
                {
                    j = i;
                    xmin = res[(i)- 1];
                    iout = s[(i)- 1];
                }
            }

            res[(j)- 1] = res[(kk)- 1];
            s[(j)- 1] = s[(kk)- 1];
        }

        kk = kk-1;
        pivot = q[(iout)- 1+(in- 1)*klm2d];
        ii = (int)(q[(iout)- 1+(n2- 1)*klm2d]);
        if (iphase != 1)
        {
            if (ii < 0)
            {
                Dummy.go_to("Dcl1",390);
            }

            if (iu[1+(ii- 1)*2] == 1)
            {
                Dummy.go_to("Dcl1",420);
            }

            Dummy.go_to("Dcl1",400);

            label390:
               Dummy.label("Dcl1",390);

            if (iu[(-ii - 1)*2] == 1)
            {
                Dummy.go_to("Dcl1",420);
            }
        }
        label400:
           Dummy.label("Dcl1",400);
        ii = Math.abs(ii);
        cuv = cu[(ii- 1)*2]+cu[1+(ii- 1)*2];
        if (q[(klm1)- 1+(in- 1)*klm2d]-pivot*cuv <= toler)
            Dummy.go_to("Dcl1",420);
        // C BYPASS INTERMEDIATE VERTICES.

        for (j = js; j <= n1; j++)
        {
            z = q[(iout)- 1+(j- 1)*klm2d];
            q[(klm1)- 1+(j- 1)*klm2d] =
                    q[(klm1)- 1+(j- 1)*klm2d]-z*cuv;
            q[(iout)- 1+(j- 1)*klm2d] = -z;
        }

        q[(iout)- 1+(n2- 1)*klm2d] = -q[(iout)- 1+(n2- 1)*klm2d];
        Dummy.go_to("Dcl1",350);

        //----------------------------------------------------------
        //
        // C GAUSS-JORDAN ELIMINATION.
        //
        label420:
           Dummy.label("Dcl1",420);

//		System.out.println("Gauss Jordan elimination");
//		start = System.currentTimeMillis();

        if (iter.val >= maxit)
        {
            kode.val = 3;
            prepare_output(x);
            return;
        }

//		System.out.println("Before first loop");
        iter.val++;

        { int tt = iout-1-klm2d;
          int ttt = in*klm2d;
          int tttt = n1 * klm2d;
        for (j = js*klm2d; j <= tttt; j=j+klm2d) {
            if (j != ttt)
            {
                q[tt+j] /=  pivot;
            }
        }
        }

//		end = System.currentTimeMillis();
//		System.out.println("end first loop " + (double)(end-start)/1000.0 + " ms");

        // C IF PERMITTED, USE SUBROUTINE COL OF THE DESCRIPTION
        // C SECTION AND REPLACE THE FOLLOWING SEVEN STATEMENTS DOWN
        // C TO AND INCLUDING STATEMENT NUMBER 460 BY..
        // C     DO 460 J=JS,N1
        // C        IF(J .EQ. IN) GO TO 460
        // C        Z = -Q(IOUT,J)
        // C        CALL COL(Q(1,J), Q(1,IN), Z, IOUT, KLM1)
        // C 460 CONTINUE

//		System.out.println("Before second loop");
        int temp3 = (in-1)*klm2d;
        int temp4 = (n2-1)*klm2d;

        //System.gc();

        {
        int tt = 0;
        for (int j = js; j <= n1; j++)
        {
            if (j != in)
            {
                tt = (j- 1)*klm2d;
                z = -q[(iout)- 1+tt];

                for (int i = 0; i < klm1; i++)
                {
                    if((i+1) != iout)
                    {
//						q[(i)- 1+tt] += z*q[(i)- 1+temp3];
                        q[i+tt] = q[i+tt] + z*q[i+temp3];
                    }
                }
            }
        }
        }

//		System.out.println("Before third loop");
//		end = System.currentTimeMillis();
//		System.out.println("end second loop " + (double)(end-start)/1000.0 + " ms and n1 is " + n1 + " klm1 " + klm1);


        tpivot = -pivot;

        {
            int tt = (in-1)*klm2d;
            for (i = 0; i < klm1; i++)
            {
                if (i != (iout-1))
                {
                    q[i+tt] /= tpivot;
                }
            }
        }

        q[(iout)- 1+temp3] = 1./pivot;
        z = q[(iout)- 1+temp4];
        q[(iout)- 1+temp4] = q[(klm2)- 1+temp3];
        q[(klm2)- 1+temp3] = z;
        ii = (int)Math.abs(z);

//		System.out.println("Before fourth loop");
//		end = System.currentTimeMillis();
//		System.out.println("end third loop " + (double)(end-start)/1000.0 + " ms");

        if (iu[(ii- 1)*2] != 0 && iu[1+(ii- 1)*2] != 0)
        {
            for (i = 1; i <= klm2; i++)
            {
                z = q[(i)- 1+temp3];
                q[(i)- 1+temp3] = q[(i)- 1+(js- 1)*klm2d];
                q[(i)- 1+(js- 1)*klm2d] = z;
            }

            js++;
        }

//		end = System.currentTimeMillis();
//		System.out.println("Dunzo " + (double)(end-start)/1000.0 + " ms");
        Dummy.go_to("Dcl1",240);

        //------------------------------------------------------
        //
        // C TEST FOR OPTIMALITY.
        //
        label490:
           Dummy.label("Dcl1",490);

//		System.out.println("Test for optimality.");

        if (kforce == 0)
            Dummy.go_to("Dcl1",580);
        if (iphase == 1 && q[(klm1)- 1+(n1- 1)*klm2d] <= toler)
            Dummy.go_to("Dcl1",500);
        kforce = 0;
        Dummy.go_to("Dcl1",240);

        //-----------------------------------------------------
        //
        // C SET UP PHASE 2 COSTS.
        //
        label500:
           Dummy.label("Dcl1",500);

//		System.out.println("Setup phase 2 costs.");

        iphase = 2;

        Arrays.fill(cu, 0.0);
        //for (j = 1; j <= nklm; j++) {
        //	cu[(j- 1)*2] = 0.;
        //	cu[1+(j- 1)*2] = 0.;
        //}


        for (j = n1; j <= nk; j++) {
            cu[(j- 1)*2] = 1.;
            cu[1+(j- 1)*2] = 1.;
        }


        for (i = 1; i <= klm; i++)
        {
            ii = (int)(q[(i)- 1+(n2- 1)*klm2d]);
            if (ii <= 0)
            {
                ii = -ii;
                if (iu[1+(ii- 1)*2] == 0)
                    Dummy.go_to("Dcl1",560);
                cu[1+(ii- 1)*2] = 0.;
                Dummy.go_to("Dcl1",540);
            }

            label530:
               Dummy.label("Dcl1",530);
            if (iu[(ii- 1)*2] == 0)
                Dummy.go_to("Dcl1",560);
            cu[(ii- 1)*2] = 0.;
            label540:
               Dummy.label("Dcl1",540);

            ia++;

            for (j = 1; j <= n2; j++) {
                z = q[(ia)- 1+(j- 1)*klm2d];
                q[(ia)- 1+(j- 1)*klm2d] = q[(i)- 1+(j- 1)*klm2d];
                q[(i)- 1+(j- 1)*klm2d] = z;
            }

            Dummy.label("Dcl1",560);
        }              //  Close for() loop.

        Dummy.go_to("Dcl1",160);

        label570:
           Dummy.label("Dcl1",570);
        if (q[(klm1)- 1+(n1- 1)*klm2d] <= toler)
            Dummy.go_to("Dcl1",500);
        kode.val = 1;

        // Trick the dumb thing into allowing me to use
        // the return.
        if( kode.val == 1 )
        {
            prepare_output(x);
            return;
        }

        label580:
           Dummy.label("Dcl1",580);
        if (iphase == 1)
            Dummy.go_to("Dcl1",570);

        //-----------------------------------------------------
        //
        // C PREPARE OUTPUT.
        //
        kode.val = 0;
        prepare_output(x);
        return;
    }

    private void prepare_output(double[] x)
    {
//		System.out.println("prepare output");
        sum = 0.e0;
        Arrays.fill(x, 0.0);
        Arrays.fill(res, 0.0);

        for (i = 1; i <= klm; i++)
        {
            ii = (int)(q[(i)- 1+(n2- 1)*klm2d]);
            sn = 1.0;
            if (ii <= 0)
            {
                ii = -ii;
                sn = -1.;
            }

            if (ii > n)
            {
                iimn = ii-n;
                res[(iimn)- 1] = sn*q[(i)- 1+(n1- 1)*klm2d];
                if (ii >= n1 && ii <= nk)
                {
                    sum += (double)(q[(i)- 1+(n1- 1)*klm2d]);
                }
            }
            else
            {
                x[(ii)- 1] = sn*q[(i)- 1+(n1- 1)*klm2d];
            }
        }

        error.val = sum;

        System.arraycopy(x, 0, this.x, 0, x.length);
        return;
   }
} // End class.
