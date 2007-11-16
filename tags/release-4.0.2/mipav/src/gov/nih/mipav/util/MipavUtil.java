package gov.nih.mipav.util;

public class MipavUtil {
    public static int getAvailableCores(){
        return Runtime.getRuntime().availableProcessors();
    }
    
    public static void main(String[] argv){
        System.out.println(MipavUtil.getAvailableCores());
    }
    
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
                        rdata[k + indices[indices[i]]*ydim + sliceLength*j] = rdata[k + indices[indices[i]]*ydim + sliceLength*j];
                        idata[k + indices[indices[i]]*ydim + sliceLength*j] = idata[k + indices[indices[i]]*ydim + sliceLength*j];
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
        
        for(int i = 0; i < l; i++){
            System.out.println(indices[i]);
        }
        return indices;
    }
}
