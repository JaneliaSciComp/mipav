package gov.nih.mipav.util;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

public class MipavUtil {
    public static final int nthreads = 16;
    public static final Executor threadPool = Executors.newFixedThreadPool(nthreads);
    
    /**
     * Return the available processors in your machine.
     */
    public static int getAvailableCores(){
        return Runtime.getRuntime().availableProcessors();
    }
    
    public static void main(String[] argv){
        System.out.println(MipavUtil.getAvailableCores());
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
    		ret >>= 1;
    	}
    	return ret;
    }
    
	public static void shift(float[] srcData, float[] desData, int x, int y, int srcXDim, int srcYDim, int desXDim, int desYDim){
		int sliceLen = srcXDim * srcYDim;
		for(int i = 0; i < srcData.length; i += sliceLen){
			
		}
	}
    
	public static void shift(float[] srcData, float[] desData, int x, int y, int z, int srcXDim, int srcYDim, int srcZDim, int desXDim, int desYDim, int desZDim){
		
	}

}