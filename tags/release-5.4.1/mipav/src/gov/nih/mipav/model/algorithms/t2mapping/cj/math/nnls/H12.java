package gov.nih.mipav.model.algorithms.t2mapping.cj.math.nnls;

import java.lang.*;
import gov.nih.mipav.model.algorithms.t2mapping.org.netlib.util.*;


class H12 {

// C      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
static double cl= 0.0;
static double sm= 0.0;
static double sm1= 0.0;
static double clinv= 0.0;
static double b= 0.0;
static int mdim= 0;
static int i1= 0;
static int i2= 0;
static int i3= 0;
static int i4= 0;
static int i= 0;
static int j= 0;
static int incr= 0;
static int temp_i, temp_j;
static double temp;

// C$$$$$ CALLS NO OTHER ROUTINES 
// C  CONSTRUCTION AND APPLICATION OF HOUSEHOLDER TRANSFORMATION FOR
// C  MODIFIED FROM  H12  IN LAWSON+HANSEN - SOLVING LEAST SQUARES 
// C  - PRENTICE-HALL 1974 (PP308,309).  DOUBLE PRECISION THROUGHOUT 

public final static void h12 (int mode,
int lpivot,
int l1,
int m,
double [] u, int _u_offset,
int iue,
doubleW up,
double [] c, int _c_offset,
int ice,
int icv,
int ncv)  {

final int temp_lpi = (lpivot-1)*iue+_u_offset;

if (0 >= lpivot || lpivot >= l1 || l1 > m)  
    Dummy.go_to("H12",999999);
cl = Math.abs(u[temp_lpi]);
if (mode == 2)  
    Dummy.go_to("H12",60);
// C  CONSTRUCT TRANSFORMATION 
{
for (j = l1, temp_j = (j-1)*iue+_u_offset; j <= m; j++, temp_j+=iue) {
cl = Math.max(Math.abs(u[temp_j]), cl) ;
}              //  Close for() loop. 
}
if ((cl) <= 0)  
      Dummy.go_to("H12",130);
else   Dummy.go_to("H12",20);
label20:
   Dummy.label("H12",20);
clinv = 1.0/cl;
//clinv = 1.0/(cl*cl);
//sm = Math.pow(u[temp_lpi]*clinv, 2);
//sm = u[temp_lpi]*u[temp_lpi]*clinv;
temp = u[temp_lpi]*clinv;
sm = temp*temp;
{
for (j = l1, temp_j = (j-1)*iue+_u_offset; j <= m; j++, temp_j+=iue) {
//sm = Math.pow((u[temp_j]*clinv), 2)+sm;
//sm = sm + u[temp_j]*u[temp_j]*clinv;
temp = u[temp_j]*clinv;
sm = sm + temp*temp;
}              //  Close for() loop. 
}
sm1 = sm;
cl = Math.sqrt(sm1)*cl;
if ((u[temp_lpi]) <= 0)  
      Dummy.go_to("H12",50);
else   Dummy.go_to("H12",40);
label40:
   Dummy.label("H12",40);
cl = -cl;
label50:
   Dummy.label("H12",50);
up.val = u[temp_lpi]-cl;
u[temp_lpi] = cl;
Dummy.go_to("H12",70);
// C  APLLY TRANSFORMATION 
label60:
   Dummy.label("H12",60);
if ((cl) <= 0)  
      Dummy.go_to("H12",130);
else   Dummy.go_to("H12",70);
label70:
   Dummy.label("H12",70);
if (ncv <= 0)  
    Dummy.go_to("H12",999999);
b = up.val*u[temp_lpi];
if ((b) < 0)  
      Dummy.go_to("H12",80);
else   Dummy.go_to("H12",130);
label80:
   Dummy.label("H12",80);
b = 1.0/b;
i2 = 1-icv+ice*(lpivot-1);
incr = ice*(l1-lpivot);
{
forloop120:
for (j = 1; j <= ncv; j++) {
i2 = i2+icv;
i3 = i2+incr;
i4 = i3;
sm = c[(i2)- 1+ _c_offset]*up.val;
{
/*for (i = l1, temp_i = (i-1)*iue+_u_offset; i <= m; i++, temp_i=temp_i+iue) {
sm = c[(i3)- 1+ _c_offset]*u[temp_i]+sm;
i3 = i3 + ice;
}              //  Close for() loop.*/
for (i = l1; i <= m; i++) {
sm = c[(i3)- 1+ _c_offset]*u[(i-1)*iue+_u_offset]+sm;
i3 = i3 + ice;
}              //  Close for() loop.*/
}
if ((sm) != 0)  
      Dummy.go_to("H12",100);
else
      Dummy.go_to("H12",120);
label100:
   Dummy.label("H12",100);
sm *= b;
c[(i2)- 1+ _c_offset] = sm*up.val + c[(i2)-1+_c_offset];
{
for (i = l1, temp_i = (i-1)*iue+_u_offset; i <= m; i++, temp_i+=iue) {
c[(i4)- 1+ _c_offset] = sm*u[temp_i] + c[(i4)-1+_c_offset];
i4 = i4 + ice;
}              //  Close for() loop. 
/* for (i = l1; i <= m; i++) {
c[(i4)- 1+ _c_offset] = sm*u[(i-1)*iue+_u_offset] + c[(i4)-1+_c_offset];
i4 = i4 + ice;
}              //  Close for() loop.*/
}
   Dummy.label("H12",120);
}              //  Close for() loop. 
}
label130:
   Dummy.label("H12",130);
Dummy.go_to("H12",999999);
Dummy.label("H12",999999);
return;
   }
} // End class.
