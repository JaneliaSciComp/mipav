package gov.nih.mipav.model.algorithms.t2mapping.cj.math.nnls;

import java.lang.*;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;


class Nnls {

// c      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
// C$$$$$$ CALLS Nnls,G1,G2 
// C NON-NEGATIVE LEAST-SQUARES FITTING ROUTINE.  GIVEN BY LAWSON + HANSON // C  IN  SOLVING LEAST SQUARES PROBLEMS ,PRENTICE-HALL 1974. 
// C  GIVEN  MATRIX  A  (M BY N), AND AN M-VECTOR  B  , FINDS THE N-VECTOR // C  X  THAT SOLVES THE PROBLEM 2-NORM(A*X - B) MINIMUM WITH X.GE.0 
// C 
// C  A(),MDIM,M,N  A  IS THE ARRAY, WITH MROWS,  N  COLS.  MDIM  IS THE 
// C     ACTUAL DIMENSION GIVEN  A  IN THE CALLING PROGRAM (MDIM.GE.M) 
// C     A  IS DESTROYED BY PROGRAM. 
// C  B()  ON ENTRY MUST CONTAIN M-VECTOR  B.  DESTROYED BY PROGRAM 
// C  X()  SOLUTION VECTOR  X.  NEED NOT BE INITIALIZED. 
// C  RNORM  MINIMUM TWO-NORM. 
// C  W()  AN ARRAY OF WORKING SPACE.  MUST BE DIMENSIONED AT LEAST N 
// C  ZZ()  ANOTHER WORKING ARRAY.  MUST BE DIMENSIONED AT LEAST M. 
// C  INDEX  ANOTHER WORKING ARRAY. MUST BE DIMENSIONED AT LEAST  N. 
// C  MODE  A FLAG INDICATENG OUTCOME OF SUBROUTINE. 
// C     MODE=1   MEANS ROUTINE WORKED PROPERLY. 
// C     MODE=2   MEANS DIMENSIONS OF ARRAYS WERE BAD (M.LE0 .OR. N.LE.0) 
// C     MODE=3   MEANS ITERATION COUNT EXCEEDED (MORE THAN 3*N ITERATI
// C 
static double [] dummy= new double[(1)];
static double factor= 0.0;
static double two= 0.0;
static double sm= 0.0;
static double wmax= 0.0;
static doubleW up= new doubleW(0.0);
static doubleW cc= new doubleW(0.0);
static doubleW ss= new doubleW(0.0);
static double alpha= 0.0;
static double t= 0.0;
static double asave= 0.0;
static double ztest= 0.0;
static double temp= 0.0;
static double unorm= 0.0;
static int iter= 0;
static int itmax= 0;
static int i= 0;
static int iz2= 0;
static int iz1= 0;
static int nsetp= 0;
static int npp1= 0;
static int izmax= 0;
static int iz= 0;
static int l= 0;
static int j= 0;
static int ii= 0;
static int jj= 0;
static int ip= 0;
static int next= 0;
static int jz= 0;
// C 

public static void nnls (double [] a, int _a_offset,
int mdim,
int m,
int n,
double [] b, int _b_offset,
double [] x, int _x_offset,
doubleW rnorm,
double [] w, int _w_offset,
double [] zz, int _zz_offset,
int [] index, int _index_offset,
intW mode)  {

two = 2.;
factor = 0.01;
// C 
mode.val = 1;
if (m > 0 && n > 0)  
    Dummy.go_to("Nnls",10);
mode.val = 2;
Dummy.go_to("Nnls",999999);
label10:
   Dummy.label("Nnls",10);
iter = 0;
itmax = 3*n;
// C 
// C 
// C  INITIALIZE ARRAYS INDEX  AND  X 
{
forloop20:
for (i = 1; i <= n; i++) {
x[(i)- 1+ _x_offset] = 0.0;
Dummy.label("Nnls",20);
index[(i)- 1+ _index_offset] = i;
}              //  Close for() loop. 
}
// C 
iz2 = n;
iz1 = 1;
nsetp = 0;
npp1 = 1;
// C     C  MAIN LOOP BEGINS HERE 
label30:
   Dummy.label("Nnls",30);
// C 
// C  QUIT IF ALL COEFFS ARE ALREADY IN SOLUTION. OR IF M COLS OF A HAVE 
// C  BEEN TRIANGULARIZED. 
if (iz1 > iz2 || nsetp >= m)  
    Dummy.go_to("Nnls",350);
// C 
// C 
// C  COMPUT COMPONENTS OF DUAL VECTOR W 
{
forloop50:
for (iz = iz1; iz <= iz2; iz++) {
j = index[(iz)- 1+ _index_offset];
sm = 0.0;
{
forloop40:
for (l = npp1; l <= m; l++) {
Dummy.label("Nnls",40);
sm = a[(l)- 1+(j- 1)*mdim+ _a_offset]*b[(l)- 1+ _b_offset]+sm;
}              //  Close for() loop. 
}
Dummy.label("Nnls",50);
w[(j)- 1+ _w_offset] = sm;
}              //  Close for() loop. 
}
// C  FIND LARGEST +VE W(J) 
label60:
   Dummy.label("Nnls",60);
wmax = 0.0;
{
forloop70:
for (iz = iz1; iz <= iz2; iz++) {
j = index[(iz)- 1+ _index_offset];
if (w[(j)- 1+ _w_offset] <= wmax)  
    Dummy.go_to("Nnls",70);
wmax = w[(j)- 1+ _w_offset];
izmax = iz;
Dummy.label("Nnls",70);
}              //  Close for() loop. 
}
// C 
// C 
// C 
// C  IF WMAX.LE.0 GO TO TERMINATION (KUHN-TUCKER CONDITIONS OK) 
if ((wmax) < 0)  
      Dummy.go_to("Nnls",350);
else if ((wmax) == 0)  
      Dummy.go_to("Nnls",350);
else   Dummy.go_to("Nnls",80);
label80:
   Dummy.label("Nnls",80);
iz = izmax;
j = index[(iz)- 1+ _index_offset];
// C 
// C 
// C 
// C  DSIGN OF W(J) IS OK FOR J TO BE MOVED TO SET  P. 
// C  BEGIN HOUSE-HOLDER TRANS. CHECK NEW DIAGONAL ELEMENT TO AVOID NEAR 
asave = a[(npp1)- 1+(j- 1)*mdim+ _a_offset];
H12.h12(1,npp1,npp1+1,m,a,(j- 1)*mdim+ _a_offset,1,up,dummy,0,1,1,0);
unorm = 0.0;
if (nsetp == 0)  
    Dummy.go_to("Nnls",100);
{
forloop90:
for (l = 1; l <= nsetp; l++) {
Dummy.label("Nnls",90);
unorm = Math.pow(a[(l)- 1+(j- 1)*mdim+ _a_offset], 2)+unorm;
}              //  Close for() loop. 
}
label100:
   Dummy.label("Nnls",100);
unorm = Math.sqrt(unorm);
temp = unorm+Math.abs(a[(npp1)- 1+(j- 1)*mdim+ _a_offset])*factor;
if ((temp-unorm) < 0)  
      Dummy.go_to("Nnls",130);
else if ((temp-unorm) == 0)  
      Dummy.go_to("Nnls",130);
else   Dummy.go_to("Nnls",110);
// C  COL J IS SUFFICIENTLY INDEP. COPY  B  TO ZZ. UPDATE  Z  AND SOLVE 
// C  FOR  ZTEST (=PROPOSED NEW VAL FOR X(J)) 
// C 
label110:
   Dummy.label("Nnls",110);
{
forloop120:
for (l = 1; l <= m; l++) {
Dummy.label("Nnls",120);
zz[(l)- 1+ _zz_offset] = b[(l)- 1+ _b_offset];
}              //  Close for() loop. 
}
H12.h12(2,npp1,npp1+1,m,a,(j- 1)*mdim+ _a_offset,1,up,zz,_zz_offset,1,1,1);
ztest = zz[(npp1)- 1+ _zz_offset]/a[(npp1)- 1+(j- 1)*mdim+ _a_offset];
// C 
// C 
// C              SEE IF  ZTEST IS +VE 
if ((ztest) < 0)  
      Dummy.go_to("Nnls",130);
else if ((ztest) == 0)  
      Dummy.go_to("Nnls",130);
else   Dummy.go_to("Nnls",140);
// C 
// C  REJECT  J  AS A CANDIDATE TO BE MOVED FROM SET  Z  TO SET  P . 
// C  RESTORE A(NPP1,J), SET W(J)=0. AND LOOP BACK TO TEST DUAL 
// C  COEFFFS AGAIN. 
// C 
label130:
   Dummy.label("Nnls",130);
a[(npp1)- 1+(j- 1)*mdim+ _a_offset] = asave;
w[(j)- 1+ _w_offset] = 0.0;
Dummy.go_to("Nnls",60);
// C 
// C  THE INDEX J=INDEX(IZ) HAS BEEN SELECTED TO BE MOVED FTOM SET  Z 
// C  TO SET  P .  UPDATE B,  UPDATE INDICES, APPLY HOUSEHOLDER TRANS 
// C  TO COLS IN NEW SET Z.  ZERO SUBDIAGONAL  ELTS IN COL  J, SET 
// C  W(J)=0. 
// C 
label140:
   Dummy.label("Nnls",140);
{
forloop150:
for (l = 1; l <= m; l++) {
Dummy.label("Nnls",150);
b[(l)- 1+ _b_offset] = zz[(l)- 1+ _zz_offset];
}              //  Close for() loop. 
}
// C 
index[(iz)- 1+ _index_offset] = index[(iz1)- 1+ _index_offset];
index[(iz1)- 1+ _index_offset] = j;
iz1 = 1+iz1;
nsetp = npp1;
npp1 = 1+npp1;
// C 
if (iz1 > iz2)  
    Dummy.go_to("Nnls",170);
{
forloop160:
for (jz = iz1; jz <= iz2; jz++) {
jj = index[(jz)- 1+ _index_offset];
Dummy.label("Nnls",160);
H12.h12(2,nsetp,npp1,m,a,(j- 1)*mdim+ _a_offset,1,up,a,(jj- 1)*mdim+ _a_offset,1,mdim,1);
}              //  Close for() loop. 
}
label170:
   Dummy.label("Nnls",170);
// C 
if (nsetp == m)  
    Dummy.go_to("Nnls",190);
{
forloop180:
for (l = npp1; l <= m; l++) {
Dummy.label("Nnls",180);
a[(l)- 1+(j- 1)*mdim+ _a_offset] = 0.0;
}              //  Close for() loop. 
}
label190:
   Dummy.label("Nnls",190);
// C 
w[(j)- 1+ _w_offset] = 0.0;
// C  SOLVE TRIANGULAT SYSTEM. STORE SOL IN ZZ,TEMPORARILY 
// C 
next = 1;
Dummy.go_to("Nnls",400);
label200:
   Dummy.label("Nnls",200);
// C 
// C 
// C 
// C 
// C  SECONDARY LOOP BEFINS HERE 
label210:
   Dummy.label("Nnls",210);
iter = 1+iter;
if (iter <= itmax)  
    Dummy.go_to("Nnls",220);
mode.val = 3;
// C  WRITE STATEMENT HERE DELETED********************* 
Dummy.go_to("Nnls",350);
label220:
   Dummy.label("Nnls",220);
// C 
// C 
// C 
// C  SEE IF ALL NEW CONSTRAINED COEFFS ARE FEASIBLE. IF NOT, FIND ALPHA 
alpha = two;
{
forloop240:
for (ip = 1; ip <= nsetp; ip++) {
l = index[(ip)- 1+ _index_offset];
if ((zz[(ip)- 1+ _zz_offset]) < 0)  
      Dummy.go_to("Nnls",230);
else if ((zz[(ip)- 1+ _zz_offset]) == 0)  
      Dummy.go_to("Nnls",230);
else   Dummy.go_to("Nnls",240);
// C 
label230:
   Dummy.label("Nnls",230);
t = -x[(l)- 1+ _x_offset]/(zz[(ip)- 1+ _zz_offset]-x[(l)- 1+ _x_offset]);
if (alpha <= t)  
    Dummy.go_to("Nnls",240);
alpha = t;
jj = ip;
Dummy.label("Nnls",240);
}              //  Close for() loop. 
}
// C  IF ALL NEW CONSTRAINED COEFFS ARE FEASIBLE, ALPHA IS STILL 2. IF S
// C  EXIT INTO MAIN LOOP. 
// C 
// C 
if (alpha == two)  
    Dummy.go_to("Nnls",330);
// C  OTHERWISE ALPHA WILL BE IN (0,1) TO INTERPOLATE BETWEEN OLDX 
// C 
// C 
// C 
{
forloop250:
for (ip = 1; ip <= nsetp; ip++) {
l = index[(ip)- 1+ _index_offset];
Dummy.label("Nnls",250);
x[(l)- 1+ _x_offset] = alpha*(zz[(ip)- 1+ _zz_offset]-x[(l)- 1+ _x_offset])+x[(l)- 1+ _x_offset];
}              //  Close for() loop. 
}
// C 
// C  MODIFY  A  BB  AND THE INDEX ARRAYS TO MOVE COEFF  I 
// C  FROM  SET  P  TO SET  Z. 
// C 
i = index[(jj)- 1+ _index_offset];
label260:
   Dummy.label("Nnls",260);
x[(i)- 1+ _x_offset] = 0.0;
// C 
if (jj == nsetp)  
    Dummy.go_to("Nnls",290);
jj = 1+jj;
{
forloop280:
for (j = jj; j <= nsetp; j++) {
ii = index[(j)- 1+ _index_offset];
index[(j-1)- 1+ _index_offset] = ii;
g1_adapter(a[(j-1)- 1+(ii- 1)*mdim+ _a_offset],a[(j)- 1+(ii- 1)*mdim+ _a_offset],cc,ss,a,(j-1)- 1+(ii- 1)*mdim+ _a_offset);
a[(j)- 1+(ii- 1)*mdim+ _a_offset] = 0.0;
{
forloop270:
for (l = 1; l <= n; l++) {
if (l != ii)  
    g2_adapter(cc.val,ss.val,a,(j-1)- 1+(l- 1)*mdim+ _a_offset,a,(j)- 1+(l- 1)*mdim+ _a_offset);
Dummy.label("Nnls",270);
}              //  Close for() loop. 
}
Dummy.label("Nnls",280);
g2_adapter(cc.val,ss.val,b,(j-1)- 1+ _b_offset,b,(j)- 1+ _b_offset);
}              //  Close for() loop. 
}
label290:
   Dummy.label("Nnls",290);
npp1 = nsetp;
nsetp = nsetp-1;
iz1 = iz1-1;
index[(iz1)- 1+ _index_offset] = i;
// C  ALL COEFFS IN SET P SHOULD BE FEASIBLE.  IF THEY ARE NOT
// C  TO ROUND-OFF.  NON-POSITIVE ONES SET TO 0 AND MOVED TO . 
// C 
// C 
// C 
// C 
// C 
{
forloop300:
for (jj = 1; jj <= nsetp; jj++) {
i = index[(jj)- 1+ _index_offset];
if ((x[(i)- 1+ _x_offset]) < 0)  
      Dummy.go_to("Nnls",260);
else if ((x[(i)- 1+ _x_offset]) == 0)  
      Dummy.go_to("Nnls",260);
else   Dummy.go_to("Nnls",300);
Dummy.label("Nnls",300);
}              //  Close for() loop. 
}
// C 
// C 
// C  COPY  B  TO  ZZ  AND SOLVE AGAIN AND LOOP BACK. 
{
forloop310:
for (i = 1; i <= m; i++) {
Dummy.label("Nnls",310);
zz[(i)- 1+ _zz_offset] = b[(i)- 1+ _b_offset];
}              //  Close for() loop. 
}
next = 2;
Dummy.go_to("Nnls",400);
label320:
   Dummy.label("Nnls",320);
Dummy.go_to("Nnls",210);
// C 
// C  END OF SECONDARY LOOP 
label330:
   Dummy.label("Nnls",330);
{
forloop340:
for (ip = 1; ip <= nsetp; ip++) {
i = index[(ip)- 1+ _index_offset];
Dummy.label("Nnls",340);
x[(i)- 1+ _x_offset] = zz[(ip)- 1+ _zz_offset];
}              //  Close for() loop. 
}
// C  ALL NEW COEFFS ARE +VE.  LOOP BACK TO BEGINNING. 
Dummy.go_to("Nnls",30);
// C  END OF MAIN LOOP 
// C  TERMINATING SECTION 
// C 
// C 
// C 
// C 
label350:
   Dummy.label("Nnls",350);
sm = 0.0;
if (npp1 > m)  
    Dummy.go_to("Nnls",370);
{
forloop360:
for (i = npp1; i <= m; i++) {
Dummy.label("Nnls",360);
sm = Math.pow(b[(i)- 1+ _b_offset], 2)+sm;
}              //  Close for() loop. 
}
Dummy.go_to("Nnls",390);
label370:
   Dummy.label("Nnls",370);
{
forloop380:
for (j = 1; j <= n; j++) {
Dummy.label("Nnls",380);
w[(j)- 1+ _w_offset] = 0.0;
}              //  Close for() loop. 
}
label390:
   Dummy.label("Nnls",390);
rnorm.val = Math.sqrt(sm);
Dummy.go_to("Nnls",999999);
// C 
// C  SOLUTION OF LINEAR TRIANGULAR SYSTEM 
// C 
// C 
label400:
   Dummy.label("Nnls",400);
{
forloop430:
for (l = 1; l <= nsetp; l++) {
ip = nsetp+1-l;
if (l == 1)  
    Dummy.go_to("Nnls",420);
{
forloop410:
for (ii = 1; ii <= ip; ii++) {
Dummy.label("Nnls",410);
zz[(ii)- 1+ _zz_offset] = zz[(ii)- 1+ _zz_offset]-a[(ii)- 1+(jj- 1)*mdim+ _a_offset]*zz[(ip+1)- 1+ _zz_offset];
}              //  Close for() loop. 
}
label420:
   Dummy.label("Nnls",420);
jj = index[(ip)- 1+ _index_offset];
Dummy.label("Nnls",430);
zz[(ip)- 1+ _zz_offset] = zz[(ip)- 1+ _zz_offset]/a[(ip)- 1+(jj- 1)*mdim+ _a_offset];
}              //  Close for() loop. 
}
if (next == 1) 
  Dummy.go_to("Nnls",200);
else if (next == 2) 
  Dummy.go_to("Nnls",320);
// C ******** FORMAT DELETED FOR PRINT STATEMENT HERE **************** 
Dummy.label("Nnls",999999);
return;
   }
// adapter for g1
private static void g1_adapter(double arg0 ,double arg1 ,doubleW arg2 ,doubleW arg3 ,double [] arg4 , int arg4_offset )
{
doubleW _f2j_tmp4 = new doubleW(arg4[arg4_offset]);

G1.g1(arg0,arg1,arg2,arg3,_f2j_tmp4);

arg4[arg4_offset] = _f2j_tmp4.val;
}

// adapter for g2
private static void g2_adapter(double arg0 ,double arg1 ,double [] arg2 , int arg2_offset ,double [] arg3 , int arg3_offset )
{
double tmp2=arg2[arg2_offset], tmp3=arg3[arg3_offset];

arg2[arg2_offset] = arg0 * tmp2 + arg1 * tmp3;
arg3[arg3_offset] = arg0 * tmp3 - arg1 * tmp2;
}
};
