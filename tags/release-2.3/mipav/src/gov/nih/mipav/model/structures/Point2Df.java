package gov.nih.mipav.model.structures;

/** 
*   Simple two-dimensional point structure of float type.
*
*		@version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*/



public class Point2Df extends PointND {

	/** X coordinate of point. */
    public float   x;
	/** Y coordinate of point. */
    public float   y;

    /**
    *   Initializes structure to (0,0).
    */
    public Point2Df () {
        x = 0;
        y = 0;
    }
    
    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    */
    public Point2Df (float x, float y){
        this.x = x;
        this.y = y;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    */
    public Point2Df (int x, int y){
        this.x = (float)x;
        this.y = (float)y;
    }
    
    /**
    *   Forms a vector from its self and a supplied pt.
    *   @param pt    Used to form a vector.
    *   @return      A two-dimensional vector.
    */
    public Vector2Df formVector(Point2Df pt) {
        return (new Vector2Df(x-pt.x, y-pt.y));
    }
    
    /**
    *   Calculates the Euclidean distance from its self to the supplied point.  
    *   @param pt   Point to which the distance is calculated.
    *   @return     The distance.
    */
    public final double distance(Point2Df pt){
        return (Math.sqrt((double)((x-pt.x)*(x-pt.x) + (y-pt.y)*(y-pt.y))));
    }
    
    /**
    *	Prints out this point object in a user readable form.
    *	@return	This object as a string.
    */
    public String toString() {
    	return new String("x: " + x + " y: " + y);
    }
    
}
