package gov.nih.mipav.model.structures;

/**
*   This is a simple 2D vector structure
*
*		@version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/


public class Vector2Dd {

    public double   x;
    public double   y;

    /**
    *
    */
    public Vector2Dd () {
        x = 0;
        y = 0;
    }

    /**
    *
    *   @param x    first parameter of the vector
    *   @param y    second parameter of the vector
    */
    public Vector2Dd (double x, double y){
        this.x = x;
        this.y = y;
    }

    /**
    *   Normalizes the vector
    */
    public final void normalizeVector() {
        double norm = (double)Math.sqrt((double)(x*x + y*y));
        x = x/norm;
        y = y/norm;
    }

    /**
    *  Calculates the length of the vector
    *  @return length of vector
    */
    public final double length(){
        return (double)Math.sqrt((double)(x*x + y*y));
    }

    /**
    *   Calculates the normal length of the vector
    *   @return normalized vector
    */
    public final Vector2Dd getNormVector() {
        double norm = (double)Math.sqrt((double)(x*x + y*y));

        return  (new Vector2Dd(x/norm, y/norm));
    }


    /**
    *   Forms vector product of input vector and its self.
    *   @param vect  input vector
    *   @return vector cross product ( Is used to determine (as seen from this vector) if the
    *           input vector lies to the left (result = positive), colinear (result = 0),
    *           left(result = negative);
    */
    public final double crossProductVectors(Vector2Dd vect) {
        return (x*vect.y - y*vect.x);
    }
}
