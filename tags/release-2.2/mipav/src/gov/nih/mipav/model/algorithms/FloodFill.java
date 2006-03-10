/** FloodFill.java  **************************************************
 *
 *		@version 0.1 Jan 5, 1998
 *		@author Matthew J. McAuliffe
 *
 ********************************************************************* */
package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import java.util.*;
import java.awt.*;


public class FloodFill {

    private int[] dims;
    private int nDims;
    private BitSet mask;
    private BitSet newMask;

    public FloodFill( BitSet mask, int[] dims ) {

        int length = 1;

        this.mask = mask;
        this.dims = dims;
        this.nDims = dims.length;

        for ( int i = 0; i < nDims; i++ ) {
            length *= dims[i];
        }

        newMask = new BitSet( length );
    }

    public void finalize() {

        mask = null;
        newMask = null;
        System.gc();
    }

    public BitSet floodFill3D( Point3D seedPt ) {
        int imageSize = dims[0] * dims[1];
        int xDim = dims[0];
        int yDim = dims[1];
        int zDim = dims[2];
        int x, y, z;
        int indexZ, indexY;

        Point3D pt; // = new Point3D();
        Point3D tempPt;
        Stack stack = new Stack();

        if ( mask.get( seedPt.z * imageSize + seedPt.y * xDim + seedPt.x ) ) {
            stack.push( seedPt );
            mask.clear( seedPt.z * imageSize + seedPt.y * xDim + seedPt.x );

            while ( !stack.empty() ) {
                pt = (Point3D) stack.pop();
                x = pt.x;
                y = pt.y;
                z = pt.z;

                indexZ = z * imageSize;
                indexY = y * xDim;
                newMask.set( indexZ + indexY + x );

                if ( x + 1 < xDim ) {
                    if ( mask.get( indexZ + indexY + x + 1 ) ) {
                        tempPt = new Point3D( x + 1, y, z );
                        stack.push( tempPt );
                        mask.clear( indexZ + indexY + tempPt.x );
                    }
                }
                if ( x - 1 >= 0 ) {
                    if ( mask.get( indexZ + indexY + x - 1 ) ) {
                        tempPt = new Point3D( x - 1, y, z );
                        stack.push( tempPt );
                        mask.clear( indexZ + indexY + tempPt.x );
                    }
                }
                if ( y + 1 < yDim ) {
                    if ( mask.get( indexZ + ( y + 1 ) * xDim + x ) ) {
                        tempPt = new Point3D( x, y + 1, z );
                        stack.push( tempPt );
                        mask.clear( indexZ + tempPt.y * xDim + x );
                    }
                }
                if ( y - 1 >= 0 ) {
                    if ( mask.get( indexZ + ( y - 1 ) * xDim + x ) ) {
                        tempPt = new Point3D( x, y - 1, z );
                        stack.push( tempPt );
                        mask.clear( indexZ + tempPt.y * xDim + x );
                    }
                }
                if ( z + 1 < zDim ) {
                    if ( mask.get( ( z + 1 ) * imageSize + indexY + x ) ) {
                        tempPt = new Point3D( x, y, z + 1 );
                        stack.push( tempPt );
                        mask.clear( tempPt.z * imageSize + indexY + x );
                    }
                }
                if ( z - 1 >= 0 ) {
                    if ( mask.get( ( z - 1 ) * imageSize + indexY + x ) ) {
                        tempPt = new Point3D( x, y, z - 1 );
                        stack.push( tempPt );
                        mask.clear( tempPt.z * imageSize + indexY + x );
                    }
                }
            }
        }

        return newMask;
    }

    public BitSet floodFill2D( Point seedPt ) {

        int xDim = dims[0];
        int yDim = dims[1];
        Point pt;
        Point tempPt;
        Stack stack = new Stack();
        int indexY;
        int x, y;

        if ( mask.get( seedPt.y * xDim + seedPt.x ) ) {
            stack.push( seedPt );
            mask.clear( seedPt.y * xDim + seedPt.x );

            while ( !stack.empty() ) {
                pt = (Point) stack.pop();
                x = pt.x;
                y = pt.y;
                indexY = y * xDim;

                newMask.set( indexY + x );

                if ( x + 1 < xDim ) {
                    if ( mask.get( indexY + x + 1 ) ) {
                        tempPt = new Point( x + 1, y );
                        stack.push( tempPt );
                        mask.clear( indexY + tempPt.x );
                    }
                }
                if ( x - 1 >= 0 ) {
                    if ( mask.get( indexY + x - 1 ) ) {
                        tempPt = new Point( x - 1, y );
                        stack.push( tempPt );
                        mask.clear( indexY + tempPt.x );
                    }
                }
                if ( y + 1 < yDim ) {
                    if ( mask.get( ( y + 1 ) * xDim + x ) ) {
                        tempPt = new Point( x, y + 1 );
                        stack.push( tempPt );
                        mask.clear( tempPt.y * xDim + x );
                    }
                }
                if ( y - 1 >= 0 ) {
                    if ( mask.get( ( y - 1 ) * xDim + x ) ) {
                        tempPt = new Point( x, y - 1 );
                        stack.push( tempPt );
                        mask.clear( tempPt.y * xDim + x );
                    }
                }
            }
        }

        return newMask;
    }
}
