package dtioverlay.utils;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Nov 20, 2005
 * Time: 9:21:15 AM
 * To change this template use Options | File Templates.
 * ************************************
 * Magnetic Resonance in Medicine Final Project
 * Released: December 1, 2005
 *
 * class PT
 *      Represent a 3-tuple. For use as a point, vector, etc.
 *
 * Copyright (C) 2005 Bennett Landman, bennett@bme.jhu.edu
 */
public class PT {
    public float x,y,z; // coordinates

    // create a new PT
    public PT(float x0,float y0,float z0) {
        x=x0;y=y0;z=z0;
    }

    // create a new PT
    public PT(double x0,double y0,double z0) {
        x=(float)x0;y=(float)y0;z=(float)z0;
    }

    // convert position to an Matlab coordinate index
    int cor2ind(int sx,int  sy) {
        return Math.round(x)+sx*(Math.round(y)+Math.round(z)*sy);
    }

    // convert coordinate to a string for debugging
    public String toString() {
        return "("+(x)+","+(y)+","+(z)+")";
    }

    // perform coordinate-wise equality test
    public boolean equals(PT pt) {
        return (pt.x==x)&&(pt.y==y)&&(pt.z==z);
    }

    // perform coordinate-wise subtraction
    public PT minus(PT b) {
        return new PT(x-b.x,y-b.y,z-b.z);
    }

    // perform coordinate-wise addition
    public PT plus(PT b) {
        return new PT(x+b.x,y+b.y,z+b.z);
    }

    // perform the vector cross-product
    public PT cross(PT b) {
        return new PT(
          y*b.z-z*b.y, z*b.x-x*b.z,x*b.y-y*b.x
        );
    }

    // multiply by a scalar
    public PT times(float t) {
        return new PT(x*t,y*t,z*t);
    }

    // perform the vector dot product
    public float dot(PT b) {
        return x*b.x+y*b.y+z*b.z;
    }

    // compute the L-2 norm of the vector
    public double length() {
        return Math.sqrt(x*x+y*y+z*z);
    }

}
