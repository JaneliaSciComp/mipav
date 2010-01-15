package gov.nih.mipav.util;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

public class MipavUtil {
    public static final int nthreads = 2 * getAvailableCores();
    
    public static Executor mipavThreadPool  = Executors.newFixedThreadPool(nthreads);
    
    /**
     * Return the available processors in your machine.
     * TODO: Add a similar method for GPUs once such a method exists in Java
     */
    public static int getAvailableCores(){
        return Runtime.getRuntime().availableProcessors();
    }
    
    /**
     * Swap slices in order to apply FFT algorithm.
     * @param rdata
     * @param idata
     * @param xdim
     * @param ydim
     * @param zdim
     * @param plane
     */
    public static void swapSlices(float[] rdata, float[] idata, int xdim, int ydim, int zdim, int plane){
        int[] indices = null;
        int sliceLength = xdim * ydim;

        if(plane == MipavConstants.SLICE_XY){
            indices = generateFFTIndices(zdim);
            float rtemp, itemp;
            for(int i = 0; i < indices.length; i++){
                if(indices[i] < 0 || indices[indices[i]] == indices[i]) continue;
                for(int j = 0; j < sliceLength; j++){
                    rtemp = rdata[indices[i]*sliceLength+j];
                    itemp = idata[indices[i]*sliceLength+j];
                    rdata[indices[i]*sliceLength+j] = rdata[indices[indices[i]]*sliceLength+j];
                    idata[indices[i]*sliceLength+j] = idata[indices[indices[i]]*sliceLength+j];
                    rdata[indices[indices[i]]*sliceLength+j] = rtemp;
                    idata[indices[indices[i]]*sliceLength+j] = itemp;
                }
                indices[indices[i]] = -1;
                indices[i] = -1;
            }
        }else if(plane == MipavConstants.SLICE_YZ){
            indices = generateFFTIndices(xdim);
            float rtemp, itemp;
            int step = xdim;
            for(int i = 0; i < indices.length; i++){
                if(indices[i] < 0 || indices[indices[i]] == indices[i]) continue;
                int index1 = indices[i];
                int index2 = indices[indices[i]];
                indices[indices[i]] = -1;
                indices[i] = -1;
                
                while (index1 < xdim * ydim * zdim) {
                    rtemp = rdata[index1];
                    itemp = idata[index1];
                    rdata[index1] = rdata[index2];
                    idata[index1] = idata[index2];
                    rdata[index2] = rtemp;
                    idata[index2] = itemp;
                    index1 += step;
                    index2 += step;
                }
            }
        }else if(plane == MipavConstants.SLICE_ZX){
            indices = generateFFTIndices(ydim);
            float rtemp, itemp;
            for(int i = 0; i < indices.length; i++){
                if(indices[i] < 0 || indices[indices[i]] == indices[i]) continue;
                for(int j = 0; j < zdim; j++){
                    for(int k = 0; k < xdim; k++){
                        rtemp = rdata[k + indices[i]*ydim + sliceLength*j];
                        itemp = idata[k + indices[i]*ydim + sliceLength*j];
                        rdata[k + indices[i]*ydim + sliceLength*j] = rdata[k + indices[indices[i]]*ydim + sliceLength*j];
                        idata[k + indices[i]*ydim + sliceLength*j] = idata[k + indices[indices[i]]*ydim + sliceLength*j];
                        rdata[k + indices[indices[i]]*ydim + sliceLength*j] = rtemp;
                        idata[k + indices[indices[i]]*ydim + sliceLength*j] = itemp;
                    }
                }
                indices[indices[i]] = -1;
                indices[i] = -1;
            }
        }else{
            throw new IllegalArgumentException("The value of variable plane is illeagal: " + plane);
        }       
    }

    /**
     * In order to use FFT, first thing is to rearrange the order of signals. 
     * @param l the length of one dimension FFT
     * @return  the indices used by FFT
     * @author Hailong Wang, Ph.D
     */
    public static final int[] generateFFTIndices(int l){
        int n = (int)(Math.log(l)/Math.log(2));
        int l2 = (int)Math.pow(2, n);
        if(l != l2){
            System.out.println("The value of l must be the power of 2: " + l);
            return null;
        }
        
        int[] indices = new int[l];
        for(int i = 0; i < n; i++){
            int max = (int)Math.pow(2, i);
            
            for(int j = 0; j < max; j++){
                indices[max+j] += indices[j] + (int)Math.pow(2, n-i-1); 
            }
        }
        return indices;
    }
    /**
     * Calculate the minimum power of two which is greater or equal to the number.
     * @return the minimum power of two which is greater or equal to the value
     */
    public static int findMinimumPowerOfTwo(int num){
        int ret = 1;
        while(ret < num){
            ret <<= 1;
        }
        return ret;
    }
    
    public static float min(float[] data){
        float min = Float.MAX_VALUE;
        for(int i = 0; i < data.length; i++){
            if(min > data[i]){
                min = data[i];
            }
        }
        return min;
    }
    
    public static short min(short[] data){
        short min = Short.MAX_VALUE;
        for(int i = 0; i < data.length; i++){
            if(min > data[i]){
                min = data[i];
            }
        }
        return min;
    }
    
    public static float max(float[] data){
        float max = Float.MIN_VALUE;
        for(int i = 0; i < data.length; i++){
            if(max < data[i]){
                max = data[i];
            }
        }
        return max;
    }
    
    public static short max(short[] data){
        short max = Short.MIN_VALUE;
        for(int i = 0; i < data.length; i++){
            if(max < data[i]){
                max = data[i];
            }
        }
        return max;
    }
    
    public static void copy2D(float[] srcData, int srcFrom, int srcXDim,
            int srcYDim, float[] destData, int destFrom, int destXDim,
            int destYDim, boolean source) {
        if (source) {
            for (int i = 0; i < srcYDim; i++) {
                System.arraycopy(srcData, srcFrom + i * srcXDim, destData,
                        destFrom + i * destXDim, srcXDim);
            }
        } else {
            for (int i = 0; i < srcYDim; i++) {
                System.arraycopy(srcData, srcFrom + i * srcXDim, destData,
                        destFrom + i * destXDim, destXDim);
            }
        }
    }

    public static void copy3D(float[] srcData, int srcFrom, int srcXDim,
            int srcYDim, int srcZDim, float[] destData, int destFrom,
            int destXDim, int destYDim, int destZDim, boolean source) {
        int srcSliceSize = srcXDim * srcYDim;
        int destSliceSize = destXDim * destYDim;
        if (source) {
            for (int i = 0; i < srcZDim; i++) {
                copy2D(srcData, srcFrom + i * srcSliceSize, srcXDim, srcYDim,
                        destData, destFrom + i * destSliceSize, destXDim,
                        destYDim, source);
            }
        } else {
            for (int i = 0; i < destZDim; i++) {
                copy2D(srcData, srcFrom + i * srcSliceSize, srcXDim, srcYDim,
                        destData, destFrom + i * destSliceSize, destXDim,
                        destYDim, source);
            }
        }
    }

    public static void copy4D(float[] srcData, int srcXDim, int srcYDim,
            int srcZDim, int srcTDim, float[] destData, int destXDim,
            int destYDim, int destZDim, int destTDim, boolean source) {
        int srcVolumeSize = srcXDim * srcYDim;
        int destVolumeSize = destXDim * destYDim;
        if (source) {
            for (int i = 0; i < srcTDim; i++) {
                copy3D(srcData, i * srcVolumeSize, srcXDim, srcYDim, srcZDim,
                        destData, i * destVolumeSize, destXDim, destYDim,
                        destZDim, source);
            }
        } else {
            for (int i = 0; i < destTDim; i++) {
                copy3D(srcData, i * srcVolumeSize, srcXDim, srcYDim, srcZDim,
                        destData, i * destVolumeSize, destXDim, destYDim,
                        destZDim, source);
            }
        }
    }
    
    /** Calculates phase from real and imaginary parts. */
    public static float phase(float rdata, float idata){
        return (float) java.lang.Math.atan2(idata, rdata);
    }

    /**
     * sqrt(a^2 + b^2) without under/overflow. *
     *
     * @return  sqrt(a^2 + b^2)
     */

    public static double hypot(double a, double b) {
        double r;

        if (Math.abs(a) > Math.abs(b)) {
            r = b / a;
            r = Math.abs(a) * Math.sqrt(1 + (r * r));
        } else if (b != 0) {
            r = a / b;
            r = Math.abs(b) * Math.sqrt(1 + (r * r));
        } else {
            r = 0.0;
        }

        return r;
    }

    /**
     * Copy a row in x, y or z direction to an array.
     * 
     * @param src       
     * @param srcPos
     * @param dest
     * @param destPos
     * @param length
     * @param srcDist   the distance between two pixels of source data in x, y or z direction.
     * @param destDist  the distance between two pixels of destination data in x, y or z direction.
     */
    public static void rowCopy(double[] src, int srcPos, double[] dest, int destPos, int length, int srcDist, int destDist){
        for(int i = 0; i < length; i++){
            dest[destPos + i * destDist] = src[srcPos + i * srcDist];
        }
    }

    /**
     * Calculate the size of the image.
     * @param dims
     * @return
     */
    public static int calculateImageSize(int[] dims){
        int size = 1;
        for(int i = 0; i < dims.length; i++){
            size *= dims[i];
        }
        return size;
    }

    /**
     * Copy a float array to a double array 
     * @param src       a float array
     * @param srcPos    the start position of the source array
     * @param dest      a double array
     * @param destPos   the start position of the destination array
     * @param length    the length of data to be copied
     */
    public static void arrayCopy(float[] src, int srcPos, double[] dest, int destPos, int length){
        if(src == null || dest == null){
            return;
        }
        
        if(srcPos < 0 || srcPos >= src.length|| destPos < 0 || destPos >= dest.length){
            return;
        }
        
        if((srcPos+length) > src.length || (destPos+length) > dest.length){
            return;
        }
        for(int i = 0; i < length; i++){
            dest[destPos+i] = src[i+srcPos];
        }
    }

    /**
     * Copy a double array to a float array.
     * @param src       a float array
     * @param srcPos    the start position of the source array
     * @param dest      a double array
     * @param destPos   the start position of the destination array
     * @param length    the length of data to be copied
     */
    public static void arrayCopy(double[] src, int srcPos, float[] dest, int destPos, int length){
        if(src == null || dest == null){
            return;
        }
        
        if(srcPos < 0 || srcPos >= src.length|| destPos < 0 || destPos >= dest.length){
            return;
        }
        
        if((srcPos+length) > src.length || (destPos+length) > dest.length){
            return;
        }
        for(int i = 0; i < length; i++){
            dest[destPos+i] = (float)src[i+srcPos];
        }
    }
    
    /**
     * A utility function to print out a double array.
     * @param data  a double array.
     */
    public static void print(double[] data){
        if(data == null)
            return;
        StringBuffer sb = new StringBuffer("");
        for(int i = 0; i < (data.length - 1); i++){
            sb.append(data[i]);
            sb.append(", ");
        }
        sb.append(data[data.length-1]);
        System.out.println(sb.toString());
    }
    /**
     * A utility function to print out a double array.
     * @param data  a double array.
     */
    public static void print(float[] data){
        if(data == null)
            return;
        StringBuffer sb = new StringBuffer("");
        for(int i = 0; i < (data.length - 1); i++){
            sb.append(data[i]);
            sb.append(", ");
        }
        sb.append(data[data.length-1]);
        System.out.println(sb.toString());
    }
}
