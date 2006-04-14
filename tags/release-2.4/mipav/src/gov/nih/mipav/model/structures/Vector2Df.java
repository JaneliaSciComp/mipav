package gov.nih.mipav.model.structures;

/**
*   This is a simple 2D vector structure
*
*		@version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/


public class Vector2Df {

    public float   x;
    public float   y;

    /**
    *   Default constructor.
    */
    public Vector2Df () {
        x = 0;
        y = 0;
    }

    /**
    *   Sets the initial values
    *   @param x    - first parameter of the vector
    *   @param y    - second parameter of the vector
    */
    public Vector2Df (float x, float y){
        this.x = x;
        this.y = y;
    }

    /**
    *   Normalizes the vector
    */
    public final void normalizeVector() {
        float norm = (float)Math.sqrt((double)(x*x + y*y));
        x = x/norm;
        y = y/norm;
    }

    /**
    *  Calculates the length of the vector
    *  @return length of vector
    */
    public final float length(){
        return (float)Math.sqrt((double)(x*x + y*y));
    }

    /**
    *   Calculates the normal length of the vector
    *   @return normalized vector
    */
    public final Vector2Df getNormVector() {
        float norm = (float)Math.sqrt((double)(x*x + y*y));

        return  (new Vector2Df(x/norm, y/norm));
    }


    /**
    *   Rorms vector product of input vector and its self.
    *   @param vect  input vector
    *   @return vector cross product ( Is used to determine (as seen from this vector) if the
    *           input vector lies to the left (result = positive), colinear (result = 0),
    *           left(result = negative);
    */
    public final double crossProductVectors(Vector2Df vect) {
        return (x*vect.y - y*vect.x);
    }
}
