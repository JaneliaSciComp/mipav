package gov.nih.mipav.model.structures;

/**
*   This is a simple 3D double vector structure
*
*		@version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/


public class Vector3Dd {

    public double   x;
    public double   y;
    public double   z;

    /**
    *   Default constructor
    */
    public Vector3Dd () {
        x = 0;
        y = 0;
        z = 0;
    }

    /**
    *   Sets the initial values of the 3D vector
    *   @param x    first parameter of the vector
    *   @param y    second parameter of the vector
    *   @param z    third parameter of the vector
    */
    public Vector3Dd (double x, double y, double z){
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /* returns squared length of input vector */
    public double squaredLength() {
      return ( (x * x) + (y * y) + (z * z));
    }

    /* returns length of input vector */
    public double length() {
      return (Math.sqrt(squaredLength()));
    }

    /* scales the input vector to the new length and returns it */
    public void scale(double newlen) {
      double len = length();
      if (len != 0.0) {
        x *= newlen / len;
        y *= newlen / len;
        z *= newlen / len;
      }
    }

}
