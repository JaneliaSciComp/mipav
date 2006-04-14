package gov.nih.mipav.model.structures;


/** 
*   Simple two-dimensional point structure of double type.
*
*		@version    July 1999
*		@author     Delia McGarry
*		@author		Matthew McAuliffe, Ph.D.
*
*/
public class Point2Dd extends PointND {

	/** X coordinate of point */
    public double   x;
    /** Y coordinate of point */
    public double   y;

    /**
    *   Initializes structure to (0,0).
    */
    public Point2Dd () {
        x = 0;
        y = 0;
    }
    
    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    */
    public Point2Dd (double x, double y){
        this.x = x;
        this.y = y;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    */
    public Point2Dd (int x, int y){
        this.x = (double)x;
        this.y = (double)y;
    }
    
    /**
    *   Forms a vector from its self and a supplied point.
    *   @param pt    Used to form a vector.
    *   @return      The two-dimensional vector.
    */
    public Vector2Dd formVector(Point2Dd pt) {
        return (new Vector2Dd(x-pt.x, y-pt.y));
    }
    
    /**
    *   Calculates the Euclidean distance from its self to the supplied point.  
    *   @param pt   Point to which the distance is calculated.
    *   @return     The distance.
    */
    public final double distance(Point2Dd pt){
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
