public class Constraint implements Constraints
// Used with NMSimplex
{
	double round2(double num, int precision)
 {
 	double rnum;
 	int tnum;

 	rnum = num*Math.pow(10,precision);
 	tnum = (int)(rnum < 0 ? rnum-0.5 : rnum + 0.5);
 	rnum = tnum/Math.pow(10,precision);

 	return rnum;
 }
	public void getConstrainedValues(double x[], int n) {
		// round to 2 decimal places
   int i;

   for (i=0; i<n; i++) {
     x[i] = round2(x[i],2);
   }
	}
}