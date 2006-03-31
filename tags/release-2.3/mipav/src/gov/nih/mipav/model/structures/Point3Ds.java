package gov.nih.mipav.model.structures;

/**
*   Simple 3D short point used in Watershed to reduce memory used (i.e. short is bytes).
*
*		@version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe
*/


public class Point3Ds extends PointND {

	/** X coordinate of point. */
    public short  x;
	/** Y coordinate of point. */
    public short  y;
	/** Z coordinate of point. */
    public short  z;

    /**
    *   Initializes structure to (0,0,0).
    */
    public Point3Ds () {
        x = (short)0;
        y = (short)0;
        z = (short)0;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    *	@param z	Z coordinate value.
    */
    public Point3Ds (short x, short y, short z){
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    /**
    *	Prints out this point object in a user readable form.
    *	@return	This object as a string.
    */
    public String toString() {
    	return new String("x: " + x + " y: " + y + " z: " + z);
    }

}
