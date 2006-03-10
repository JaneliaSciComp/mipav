package gov.nih.mipav.model.structures;

import java.io.*;

/**
*
*   Simple float 4D point.
*
*       @version    0.1 Oct 27, 1997
*		@author     Matthew J. McAuliffe
*
*/


public class Point4Df extends PointND  {

	/** W coordinate of point. */
    public float   w;
	/** X coordinate of point. */
    public float   x;
	/** Y coordinate of point. */
    public float   y;
	/** Z coordinate of point. */
    public float   z;

    /**
    *   Initializes structure to (0,0,0,0).
    */
    public Point4Df () {
        w = 0;
        x = 0;
        y = 0;
        z = 0;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param w	W coordinate value.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    *	@param z	Z coordinate value.
    */
    public Point4Df (float w, float x, float y, float z){
        this.w = w;
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
    *   Initializes structure to the supplied values.
    *	@param w	W coordinate value.
    *	@param x	X coordinate value.
    *	@param y	Y coordinate value.
    *	@param z	Z coordinate value.
    */
    public Point4Df (int w, int x, int y, int z){
        this.w = (float)w;
        this.x = (float)x;
        this.y = (float)y;
        this.z = (float)z;
    }

    /**
    *	Prints out this point object in a user readable form.
    *	@return	This object as a string.
    */
    public String toString() {
    	return new String("w: " + w + " x: " + x + " y: " + y + " z: " + z);
    }
}
