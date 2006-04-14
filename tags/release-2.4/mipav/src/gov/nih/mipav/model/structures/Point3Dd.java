package gov.nih.mipav.model.structures;

/** 
*		
*   Simple double 3D point.   
*
*       @version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe
*
*/


public class Point3Dd extends PointND {

	/** X coordinate of point. */
    public double   x;
	/** Y coordinate of point. */
    public double   y;
	/** Z coordinate of point. */
    public double   z;

    /**
    *   Initializes structure to (0,0,0).
    */
    public Point3Dd () {
        x = 0;
        y = 0;
        z = 0;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    *	@param z	Z coordinate value.
    */
    public Point3Dd (double x, double y, double z){
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    *	@param z	Z coordinate value.
    */
    public Point3Dd (int x, int y, int z){
        this.x = (double)x;
        this.y = (double)y;
        this.z = (double)z;
    }
    
    /**
    *	Prints out this point object in a user readable form.
    *	@return	This object as a string.
    */
    public String toString() {
    	return new String("x: " + x + " y: " + y + " z: " + z);
    }
}
