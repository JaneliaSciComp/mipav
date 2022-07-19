package gov.nih.mipav.util;


/**
 * A class containing Array-related helper methods.
 */
public class ArrayUtil {
    public static void copy2D(final float[] srcData, final int srcFrom, final int srcXDim, final int srcYDim,
            final float[] destData, final int destFrom, final int destXDim, final int destYDim, final boolean source) {
        if (source) {
            for (int i = 0; i < srcYDim; i++) {
                System.arraycopy(srcData, srcFrom + i * srcXDim, destData, destFrom + i * destXDim, srcXDim);
            }
        } else {
            for (int i = 0; i < destYDim; i++) {
                System.arraycopy(srcData, srcFrom + i * srcXDim, destData, destFrom + i * destXDim, destXDim);
            }
        }
    }

    public static void copy3D(final float[] srcData, final int srcFrom, final int srcXDim, final int srcYDim,
            final int srcZDim, final float[] destData, final int destFrom, final int destXDim, final int destYDim,
            final int destZDim, final boolean source) {
        final int srcSliceSize = srcXDim * srcYDim;
        final int destSliceSize = destXDim * destYDim;
        if (source) {
            for (int i = 0; i < srcZDim; i++) {
                ArrayUtil.copy2D(srcData, srcFrom + i * srcSliceSize, srcXDim, srcYDim, destData, destFrom + i
                        * destSliceSize, destXDim, destYDim, source);
            }
        } else {
            for (int i = 0; i < destZDim; i++) {
                ArrayUtil.copy2D(srcData, srcFrom + i * srcSliceSize, srcXDim, srcYDim, destData, destFrom + i
                        * destSliceSize, destXDim, destYDim, source);
            }
        }
    }

    public static void copy4D(final float[] srcData, final int srcXDim, final int srcYDim, final int srcZDim,
            final int srcTDim, final float[] destData, final int destXDim, final int destYDim, final int destZDim,
            final int destTDim, final boolean source) {
        final int srcVolumeSize = srcXDim * srcYDim;
        final int destVolumeSize = destXDim * destYDim;
        if (source) {
            for (int i = 0; i < srcTDim; i++) {
                ArrayUtil.copy3D(srcData, i * srcVolumeSize, srcXDim, srcYDim, srcZDim, destData, i * destVolumeSize,
                        destXDim, destYDim, destZDim, source);
            }
        } else {
            for (int i = 0; i < destTDim; i++) {
                ArrayUtil.copy3D(srcData, i * srcVolumeSize, srcXDim, srcYDim, srcZDim, destData, i * destVolumeSize,
                        destXDim, destYDim, destZDim, source);
            }
        }
    }
    
    public static void copy2D(final double[] srcData, final int srcFrom, final int srcXDim, final int srcYDim,
            final double[] destData, final int destFrom, final int destXDim, final int destYDim, final boolean source) {
        if (source) {
            for (int i = 0; i < srcYDim; i++) {
                System.arraycopy(srcData, srcFrom + i * srcXDim, destData, destFrom + i * destXDim, srcXDim);
            }
        } else {
            for (int i = 0; i < destYDim; i++) {
                System.arraycopy(srcData, srcFrom + i * srcXDim, destData, destFrom + i * destXDim, destXDim);
            }
        }
    }

    public static void copy3D(final double[] srcData, final int srcFrom, final int srcXDim, final int srcYDim,
            final int srcZDim, final double[] destData, final int destFrom, final int destXDim, final int destYDim,
            final int destZDim, final boolean source) {
        final int srcSliceSize = srcXDim * srcYDim;
        final int destSliceSize = destXDim * destYDim;
        if (source) {
            for (int i = 0; i < srcZDim; i++) {
                ArrayUtil.copy2D(srcData, srcFrom + i * srcSliceSize, srcXDim, srcYDim, destData, destFrom + i
                        * destSliceSize, destXDim, destYDim, source);
            }
        } else {
            for (int i = 0; i < destZDim; i++) {
                ArrayUtil.copy2D(srcData, srcFrom + i * srcSliceSize, srcXDim, srcYDim, destData, destFrom + i
                        * destSliceSize, destXDim, destYDim, source);
            }
        }
    }

    public static void copy4D(final double[] srcData, final int srcXDim, final int srcYDim, final int srcZDim,
            final int srcTDim, final double[] destData, final int destXDim, final int destYDim, final int destZDim,
            final int destTDim, final boolean source) {
        final int srcVolumeSize = srcXDim * srcYDim;
        final int destVolumeSize = destXDim * destYDim;
        if (source) {
            for (int i = 0; i < srcTDim; i++) {
                ArrayUtil.copy3D(srcData, i * srcVolumeSize, srcXDim, srcYDim, srcZDim, destData, i * destVolumeSize,
                        destXDim, destYDim, destZDim, source);
            }
        } else {
            for (int i = 0; i < destTDim; i++) {
                ArrayUtil.copy3D(srcData, i * srcVolumeSize, srcXDim, srcYDim, srcZDim, destData, i * destVolumeSize,
                        destXDim, destYDim, destZDim, source);
            }
        }
    }

    /**
     * Copy a row in x, y or z direction to an array.
     * 
     * @param src
     * @param srcPos
     * @param dest
     * @param destPos
     * @param length
     * @param srcDist the distance between two pixels of source data in x, y or z direction.
     * @param destDist the distance between two pixels of destination data in x, y or z direction.
     */
    public static void rowCopy(final double[] src, final int srcPos, final double[] dest, final int destPos,
            final int length, final int srcDist, final int destDist) {
        for (int i = 0; i < length; i++) {
            dest[destPos + i * destDist] = src[srcPos + i * srcDist];
        }
    }

    /**
     * Copy a row in x, y or z direction to an array.
     * 
     * @param src
     * @param srcPos
     * @param dest
     * @param destPos
     * @param length
     * @param srcDist the distance between two pixels of source data in x, y or z direction.
     * @param destDist the distance between two pixels of destination data in x, y or z direction.
     */
    public static void rowCopy(final boolean[] src, final int srcPos, final boolean[] dest, final int destPos,
            final int length, final int srcDist, final int destDist) {
        for (int i = 0; i < length; i++) {
            dest[destPos + i * destDist] = src[srcPos + i * srcDist];
        }
    }
    /**
     * Copy a float array to a double array
     * 
     * @param src a float array
     * @param srcPos the start position of the source array
     * @param dest a double array
     * @param destPos the start position of the destination array
     * @param length the length of data to be copied
     */
    public static void arrayCopy(final float[] src, final int srcPos, final double[] dest, final int destPos,
            final int length) {
        if (src == null || dest == null) {
            return;
        }

        if (srcPos < 0 || srcPos >= src.length || destPos < 0 || destPos >= dest.length) {
            return;
        }

        if ( (srcPos + length) > src.length || (destPos + length) > dest.length) {
            return;
        }
        for (int i = 0; i < length; i++) {
            dest[destPos + i] = src[i + srcPos];
        }
    }

    /**
     * Copy a double array to a float array.
     * 
     * @param src a float array
     * @param srcPos the start position of the source array
     * @param dest a double array
     * @param destPos the start position of the destination array
     * @param length the length of data to be copied
     */
    public static void arrayCopy(final double[] src, final int srcPos, final float[] dest, final int destPos,
            final int length) {
        if (src == null || dest == null) {
            return;
        }

        if (srcPos < 0 || srcPos >= src.length || destPos < 0 || destPos >= dest.length) {
            return;
        }

        if ( (srcPos + length) > src.length || (destPos + length) > dest.length) {
            return;
        }
        for (int i = 0; i < length; i++) {
            dest[destPos + i] = (float) src[i + srcPos];
        }
    }

    /**
     * A utility function to print out a double array.
     * 
     * @param data a double array.
     */
    public static void print(final double[] data) {
        if (data == null) {
            return;
        }
        final StringBuffer sb = new StringBuffer("");
        for (int i = 0; i < (data.length - 1); i++) {
            sb.append(data[i]);
            sb.append(", ");
        }
        sb.append(data[data.length - 1]);
        System.out.println(sb.toString());
    }

    /**
     * A utility function to print out a double array.
     * 
     * @param data a double array.
     */
    public static void print(final float[] data) {
        if (data == null) {
            return;
        }
        final StringBuffer sb = new StringBuffer("");
        for (int i = 0; i < (data.length - 1); i++) {
            sb.append(data[i]);
            sb.append(", ");
        }
        sb.append(data[data.length - 1]);
        System.out.println(sb.toString());
    }
}
