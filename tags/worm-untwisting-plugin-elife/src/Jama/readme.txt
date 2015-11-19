Jama: a Java matrix package from NIST:
http://math.nist.gov/javanumerics/jama/

Updated source obtained from the above page July 2008, with slight
modifications to the Matrix class to support current use in Mipav. 

This package used to live in 
mipav\src\gov\nih\mipav\model\structures\jama\
prior to July 2008, but it was moved here (mipav\src\Jama\)
to reflect its status as a separate library and to support easy
updates as bug fixes are released occasionally by NIST.

To support Mipav, in the file Matrix.java, the private data members A,
m, n change to protected, and two methods are added at the bottom:

   public void setMatrix(double[][] matrix); 
   public void timesEquals(Matrix B);


Aron Helser, Geometric Tools, July 23, 2008.