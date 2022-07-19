package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import java.awt.*;

import java.util.*;


/**
 * Stack based flood-fill for 2D and 3D images. The input is a binary (BitSet) object and produces a BitSet or short
 * mask. A short mask allows for different label values for disconnected objects in a BitSet image. For example, in a
 * binarized image of cells, each cell could be labeled(filled) with a separate value. However, a short mask uses 16X
 * more memory than does the BitSet mask.
 *
 * @version  0.1 Jan 5, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmFloodFill extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final short BITSET = 0;

    /** DOCUMENT ME! */
    public static final short SHORT = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Dimensionality of the data. */
    private int[] dims;

    /** Flood value. Default in 1. */
    private short floodValue = 1;

    /** DOCUMENT ME! */
    private BitSet mask;

    /** Indicates the type of result mask: BITSET or SHORT. */
    private int mode;

    /** The number of dimensions. */
    private int nDims;

    /** DOCUMENT ME! */
    private BitSet newMask;

    /** Reference to short mask. */
    private short[] newShortMask = null;

    /** Seed point for 2D datasets. */
    private Point seed2DPt;

    /** Seed point for 3D datasets. */
    private Point3D seed3DPt;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Stack based flood-fill for 2D and 3D images.
     *
     * @param  mask  BitSet object that has the binary object(s) that needs to be filled
     * @param  dims  dimensionality of mask
     * @param  mode  BITSET or SHORT mode for result mask
     * @param  pt    seed point for the flood fill to begin for 2D image
     */
    public AlgorithmFloodFill(BitSet mask, int[] dims, int mode, Point pt) {
        int length = 1;

        this.mask = mask;
        this.dims = dims;
        this.nDims = dims.length;
        this.mode = mode;
        this.seed2DPt = pt;

        for (int i = 0; i < nDims; i++) {
            length *= dims[i];
        }

        if (mode == BITSET) {
            newMask = new BitSet(length);
        } else {
            newShortMask = new short[length];
        }
    }

    /**
     * Stack based flood-fill for 2D and 3D images.
     *
     * @param  mask  BitSet object that has the binary object(s) that needs to be filled
     * @param  dims  dimensionality of mask
     * @param  mode  BITSET or SHORT mode for result mask
     * @param  pt    seed point for the flood fill to begin for 3D image
     */
    public AlgorithmFloodFill(BitSet mask, int[] dims, int mode, Point3D pt) {
        int length = 1;

        this.mask = mask;
        this.dims = dims;
        this.nDims = dims.length;
        this.mode = mode;
        this.seed3DPt = pt;

        for (int i = 0; i < nDims; i++) {
            length *= dims[i];
        }

        if (mode == BITSET) {
            newMask = new BitSet(length);
        } else {
            newShortMask = new short[length];
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        mask = null;
        newMask = null;
        newShortMask = null;
        System.gc();
    }

    /**
     * 2D flood fill that forms a binary(BitSet) mask.
     *
     * @return  mask of flooded image
     */
    public BitSet floodFill2DBitSet() {

        int xDim = dims[0];
        int yDim = dims[1];
        Point pt;
        Point tempPt;
        Stack<Point> stack = new Stack<Point>();
        int indexY;
        int x, y;

        if (mask.get((seed2DPt.y * xDim) + seed2DPt.x)) {
            stack.push(seed2DPt);
            mask.clear((seed2DPt.y * xDim) + seed2DPt.x);

            while (!stack.empty()) {
                pt = (Point) stack.pop();
                x = pt.x;
                y = pt.y;
                indexY = y * xDim;

                newMask.set(indexY + x);

                if ((x + 1) < xDim) {

                    if (mask.get(indexY + x + 1)) {
                        tempPt = new Point(x + 1, y);
                        stack.push(tempPt);
                        mask.clear(indexY + tempPt.x);
                    }
                }

                if ((x - 1) >= 0) {

                    if (mask.get(indexY + x - 1)) {
                        tempPt = new Point(x - 1, y);
                        stack.push(tempPt);
                        mask.clear(indexY + tempPt.x);
                    }
                }

                if ((y + 1) < yDim) {

                    if (mask.get(((y + 1) * xDim) + x)) {
                        tempPt = new Point(x, y + 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.y * xDim) + x);
                    }
                }

                if ((y - 1) >= 0) {

                    if (mask.get(((y - 1) * xDim) + x)) {
                        tempPt = new Point(x, y - 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.y * xDim) + x);
                    }
                }
            }
        }

        setCompleted(true);

        return newMask;
    }

    /**
     * 2D flood fill that forms a short mask.
     *
     * @return  mask of flooded image
     */
    public short[] floodFill2DShort() {

        int xDim = dims[0];
        int yDim = dims[1];
        Point pt;
        Point tempPt;
        Stack<Point> stack = new Stack<Point>();
        int indexY;
        int x, y;

        if (mask.get((seed2DPt.y * xDim) + seed2DPt.x)) {
            stack.push(seed2DPt);
            mask.clear((seed2DPt.y * xDim) + seed2DPt.x);

            while (!stack.empty()) {
                pt = (Point) stack.pop();
                x = pt.x;
                y = pt.y;
                indexY = y * xDim;

                newShortMask[indexY + x] = floodValue;

                if ((x + 1) < xDim) {

                    if (mask.get(indexY + x + 1)) {
                        tempPt = new Point(x + 1, y);
                        stack.push(tempPt);
                        mask.clear(indexY + tempPt.x);
                    }
                }

                if ((x - 1) >= 0) {

                    if (mask.get(indexY + x - 1)) {
                        tempPt = new Point(x - 1, y);
                        stack.push(tempPt);
                        mask.clear(indexY + tempPt.x);
                    }
                }

                if ((y + 1) < yDim) {

                    if (mask.get(((y + 1) * xDim) + x)) {
                        tempPt = new Point(x, y + 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.y * xDim) + x);
                    }
                }

                if ((y - 1) >= 0) {

                    if (mask.get(((y - 1) * xDim) + x)) {
                        tempPt = new Point(x, y - 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.y * xDim) + x);
                    }
                }
            }
        }

        setCompleted(true);

        return newShortMask;
    }

    /**
     * 3D flood fill that forms a binary(BitSet) mask.
     *
     * @return  mask of flooded image
     */
    public BitSet floodFill3DBitSet() {
        int imageSize = dims[0] * dims[1];
        int xDim = dims[0];
        int yDim = dims[1];
        int zDim = dims[2];
        int x, y, z;
        int indexZ, indexY;

        Point3D pt; // = new Point3D();
        Point3D tempPt;
        Stack<Point3D> stack = new Stack<Point3D>();

        if (mask.get((seed3DPt.z * imageSize) + (seed3DPt.y * xDim) + seed3DPt.x)) {
            stack.push(seed3DPt);
            mask.clear((seed3DPt.z * imageSize) + (seed3DPt.y * xDim) + seed3DPt.x);

            while (!stack.empty()) {
                pt = (Point3D) stack.pop();
                x = pt.x;
                y = pt.y;
                z = pt.z;

                indexZ = z * imageSize;
                indexY = y * xDim;
                newMask.set(indexZ + indexY + x);

                if ((x + 1) < xDim) {

                    if (mask.get(indexZ + indexY + x + 1)) {
                        tempPt = new Point3D(x + 1, y, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + indexY + tempPt.x);
                    }
                }

                if ((x - 1) >= 0) {

                    if (mask.get(indexZ + indexY + x - 1)) {
                        tempPt = new Point3D(x - 1, y, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + indexY + tempPt.x);
                    }
                }

                if ((y + 1) < yDim) {

                    if (mask.get(indexZ + ((y + 1) * xDim) + x)) {
                        tempPt = new Point3D(x, y + 1, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + (tempPt.y * xDim) + x);
                    }
                }

                if ((y - 1) >= 0) {

                    if (mask.get(indexZ + ((y - 1) * xDim) + x)) {
                        tempPt = new Point3D(x, y - 1, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + (tempPt.y * xDim) + x);
                    }
                }

                if ((z + 1) < zDim) {

                    if (mask.get(((z + 1) * imageSize) + indexY + x)) {
                        tempPt = new Point3D(x, y, z + 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.z * imageSize) + indexY + x);
                    }
                }

                if ((z - 1) >= 0) {

                    if (mask.get(((z - 1) * imageSize) + indexY + x)) {
                        tempPt = new Point3D(x, y, z - 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.z * imageSize) + indexY + x);
                    }
                }
            }
        }

        setCompleted(true);

        return newMask;

    }

    /**
     * 3D flood fill that forms a short mask.
     *
     * @return  mask of flooded image
     */
    public short[] floodFill3DShort() {
        int imageSize = dims[0] * dims[1];
        int xDim = dims[0];
        int yDim = dims[1];
        int zDim = dims[2];
        int x, y, z;
        int indexZ, indexY;

        Point3D pt; // = new Point3D();
        Point3D tempPt;
        Stack<Point3D> stack = new Stack<Point3D>();

        if (mask.get((seed3DPt.z * imageSize) + (seed3DPt.y * xDim) + seed3DPt.x)) {
            stack.push(seed3DPt);
            mask.clear((seed3DPt.z * imageSize) + (seed3DPt.y * xDim) + seed3DPt.x);

            while (!stack.empty()) {
                pt = (Point3D) stack.pop();
                x = pt.x;
                y = pt.y;
                z = pt.z;

                indexZ = z * imageSize;
                indexY = y * xDim;
                newShortMask[indexZ + indexY + x] = floodValue;

                if ((x + 1) < xDim) {

                    if (mask.get(indexZ + indexY + x + 1)) {
                        tempPt = new Point3D(x + 1, y, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + indexY + tempPt.x);
                    }
                }

                if ((x - 1) >= 0) {

                    if (mask.get(indexZ + indexY + x - 1)) {
                        tempPt = new Point3D(x - 1, y, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + indexY + tempPt.x);
                    }
                }

                if ((y + 1) < yDim) {

                    if (mask.get(indexZ + ((y + 1) * xDim) + x)) {
                        tempPt = new Point3D(x, y + 1, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + (tempPt.y * xDim) + x);
                    }
                }

                if ((y - 1) >= 0) {

                    if (mask.get(indexZ + ((y - 1) * xDim) + x)) {
                        tempPt = new Point3D(x, y - 1, z);
                        stack.push(tempPt);
                        mask.clear(indexZ + (tempPt.y * xDim) + x);
                    }
                }

                if ((z + 1) < zDim) {

                    if (mask.get(((z + 1) * imageSize) + indexY + x)) {
                        tempPt = new Point3D(x, y, z + 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.z * imageSize) + indexY + x);
                    }
                }

                if ((z - 1) >= 0) {

                    if (mask.get(((z - 1) * imageSize) + indexY + x)) {
                        tempPt = new Point3D(x, y, z - 1);
                        stack.push(tempPt);
                        mask.clear((tempPt.z * imageSize) + indexY + x);
                    }
                }
            }
        }

        setCompleted(true);

        return newShortMask;
    }

    /**
     * Gets the Mask image returned as a Java Bitset object.
     *
     * @return  BitSet mask of flooded image
     */
    public BitSet getBitSetMask() {
        return newMask;
    }

    /**
     * Gets the mask image (array of short values).
     *
     * @return  short mask of flooded image
     */
    public short[] getShortMask() {
        return newShortMask;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((dims.length == 2) && (mode == SHORT)) {
            floodFill2DShort();
        } else if ((dims.length == 2) && (mode == BITSET)) {
            floodFill2DBitSet();
        } else if ((dims.length == 3) && (mode == SHORT)) {
            floodFill3DShort();
        } else if ((dims.length == 3) && (mode == BITSET)) {
            floodFill3DBitSet();
        }
    }

    /**
     * Sets this classes BitSet mask to new mask.
     *
     * @param  mask  sets BitSet mask
     */
    public void setBitSetMask(BitSet mask) {
        newMask = mask;
    }

    /**
     * Sets the value to be placed in the flood region.
     *
     * @param  floodValue  sets the flood-fill value used when generating a short type mask
     */
    public void setFloodValue(short floodValue) {
        this.floodValue = floodValue;
    }

    /**
     * Sets the seed point for the flood fill for a 2D image region grow.
     *
     * @param  seedPt  sets 2D seed point for beginning of flood fill
     */
    public void setSeed2DPt(Point seedPt) {
        this.seed2DPt = seedPt;
    }

    /**
     * Sets the point at which a the flood filling begins.
     *
     * @param  seedPt  sets 3D seed point for beginning of flood fill
     */
    public void setSeed3DPt(Point3D seedPt) {
        this.seed3DPt = seedPt;
    }

    /**
     * Sets this classes short mask to the new mask.
     *
     * @param  mask  sets short mask
     */
    public void setShortMask(short[] mask) {
        newShortMask = mask;
    }

}
