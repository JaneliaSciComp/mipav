package gov.nih.mipav.model.structures;

/**
*   This is a simple 3D vector structure
*
*		@version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/


public class Vector3Df {

    public float   x;
    public float   y;
    public float   z;

    /**
    *   Default constructor
    */
    public Vector3Df () {
        x = 0;
        y = 0;
        z = 0;
    }

    /**
    *   Sets the initial values of the vector
    *   @param x   first parameter of the vector
    *   @param y   second parameter of the vector
    *   @param z   third parameter of the vector
    */
    public Vector3Df (float x, float y, float z){
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
    *   Normalizes the vector
    */
    public final void normalizeVector() {
        float norm = (float)Math.sqrt((double)(x*x + y*y + z*z));
        x = x/norm;
        y = y/norm;
        z = z/norm;
    }

    /**
    *  Calculates the length of the vector
    *  @return length of vector
    */
    public final float length(){
        return (float)Math.sqrt((double)(x*x + y*y + z*z));
    }

    /**
    *   Calculates the normal length of the vector
    *   @return normalized vector
    */
    public final Vector3Df getNormVector() {
        float norm = (float)Math.sqrt((double)(x*x + y*y +z*z));

        return  (new Vector3Df(x/norm, y/norm, z/norm));
    }
}
