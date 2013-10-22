package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

public class AlgorithmSobel extends AlgorithmBase {
    
    private int kernelSize = 3;
    
    public AlgorithmSobel(ModelImage destImg, ModelImage srcImg, boolean image25D, int kernelSize) {
        super(destImg, srcImg);
        this.image25D = image25D;
        this.kernelSize = kernelSize;
    }
    
    public void runAlgorithm() {
        fireProgressStateChanged(0, srcImage.getImageName(), "Sobel gradient on image ...");
        if ((srcImage.getNDims() == 2) || image25D) {
            run2D();
        }
        else {
            run3D();
        }
    }
    
    public void run2D() {
        int origXDim = srcImage.getExtents()[0];
        int origYDim = srcImage.getExtents()[1];
        int xDim = origXDim - (kernelSize - 1);
        int yDim = origYDim - (kernelSize - 1);
        int zDim;
        int origSliceSize = origXDim * origYDim;
        double buffer[] = new double[origSliceSize];
        int x;
        int y;
        int z;
        int sliceSize = xDim * yDim;
        double gx[] = new double[sliceSize];
        double gy[] = new double[sliceSize];
        
        if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }
        else {
            zDim = 1;
        }
        
        for (z = 0; z < zDim; z++) {
        
            fireProgressStateChanged((100 * z)/zDim);
            try {
                srcImage.exportData(z*origSliceSize, origSliceSize, buffer);
            }
            catch (IOException e) {
                    displayError("AlgorithmSobel: IOException on srcImage.exportData(z*origSliceSize, origSliceSize, buffer)");
    
                    setCompleted(false);
    
                    return;
            }
            
            if (kernelSize == 3) {
                for (y = 1; y < origYDim - 1; y++) {
                    for (x = 1; x < origXDim - 1; x++) {
                        gx[x - 1 + (y - 1)*xDim] = buffer[x+1 + (y-1)*origXDim] - buffer[x-1 + (y-1)*origXDim]
                                                      + 2.0 * buffer[x+1 + y*origXDim] - 2.0*buffer[x-1 + y*origXDim]
                                                      + buffer[x+1 + (y+1)*origXDim] - buffer[x-1 + (y+1)*origXDim];
                        gy[x - 1 + (y - 1)*xDim] = buffer[x-1 + (y+1)*origXDim] - buffer[x-1 + (y-1)*origXDim]
                                                      + 2.0*buffer[x + (y+1)*origXDim] - 2.0*buffer[x + (y-1)*origXDim]
                                                      + buffer[x+1 + (y+1)*origXDim] - buffer[x+1 + (y-1)*origXDim];
                    } // for (x = 1; x < origXDim - 1; x++)
                } // for (y = 1; y < origYDim - 1; y++)
            } // if (kernelSize == 3)
            else if (kernelSize == 5) {
                for (y = 2; y < origYDim - 2; y++) {
                    for (x = 2; x < origXDim - 2; x++) {
                        gx[x - 2 + (y - 2)*xDim] = buffer[x+2 + (y-2)*origXDim] + 2.0 * buffer[x+1 + (y-2)*origXDim]
                                                   - 2.0 * buffer[x-1 + (y-2)*origXDim] - buffer[x-2 + (y-2)*origXDim]
                                                   + 4.0 * buffer[x+2 + (y-1)*origXDim] + 8.0 * buffer[x+1 + (y-1)*origXDim]
                                                   - 8.0 * buffer[x-1 + (y-1)*origXDim] - 4.0 * buffer[x-2 + (y-1)*origXDim]
                                                   + 6.0 * buffer[x+2 + y*origXDim] + 12.0 * buffer[x+1 + y*origXDim]
                                                   - 12.0 * buffer[x-1 + y*origXDim] - 6.0 * buffer[x-2 + y*origXDim]
                                                   + 4.0 * buffer[x+2 + (y+1)*origXDim] + 8.0 * buffer[x+1 + (y+1)*origXDim]
                                                   - 8.0 * buffer[x-1 + (y+1)*origXDim] - 4.0 * buffer[x-2 + (y+1)*origXDim]
                                                   + buffer[x+2 + (y+2)*origXDim] + 2.0 * buffer[x+1 + (y+2)*origXDim]
                                                   - 2.0 * buffer[x-1 + (y+2)*origXDim] - buffer[x-2 + (y+2)*origXDim];
                        gy[x - 2 + (y - 2)*xDim] = buffer[x-2 + (y+2)*origXDim] + 2.0 * buffer[x-2 + (y+1)*origXDim]
                                - 2.0 * buffer[x-2 + (y-1)*origXDim] - buffer[x-2 + (y-2)*origXDim]
                                + 4.0 * buffer[x-1 + (y+2)*origXDim] + 8.0 * buffer[x-1 + (y+1)*origXDim]
                                - 8.0 * buffer[x-1 + (y-1)*origXDim] - 4.0 * buffer[x-1 + (y-2)*origXDim]
                                + 6.0 * buffer[x + (y+2)*origXDim] + 12.0 * buffer[x + (y+1)*origXDim]
                                - 12.0 * buffer[x + (y-1)*origXDim] - 6.0 * buffer[x + (y-2)*origXDim]
                                + 4.0 * buffer[x+1 + (y+2)*origXDim] + 8.0 * buffer[x+1 + (y+1)*origXDim]
                                - 8.0 * buffer[x+1 + (y-1)*origXDim] - 4.0 * buffer[x+1 + (y-2)*origXDim]
                                + buffer[x+2 + (y+2)*origXDim] + 2.0 * buffer[x+2 + (y+1)*origXDim]
                                - 2.0 * buffer[x+2 + (y-1)*origXDim] - buffer[x+2 + (y-2)*origXDim];
                    } // for (x = 2; x < origXDim - 2; x++)
                } // // for (y = 2; y < origYDim - 2; y++)
            } // else if (kernelSize == 5)
            
            try {
                destImage.importData(z*sliceSize, gx, false);
            }
            catch(IOException e) {
                displayError("AlgorithmSobel: IOException on destImage.importData(z*sliceSize, gx, false)");

                setCompleted(false);

                return;  
            }
            
            try {
                destImage.importData(zDim*sliceSize + z*sliceSize, gy, false);
            }
            catch(IOException e) {
                displayError("AlgorithmSobel: IOException on destImage.importData(zDim*sliceSize + z*sliceSize, gz, false)");

                setCompleted(false);

                return;  
            }
            
        } // for (z = 0; z < zDim; z++)
        
        destImage.calcMinMax();
        setCompleted(true);
        return;
    }
    
    public void run3D() {
        int origXDim = srcImage.getExtents()[0];
        int origYDim = srcImage.getExtents()[1];
        int origZDim = srcImage.getExtents()[2];
        int xDim = origXDim - (kernelSize - 1);
        int yDim = origYDim - (kernelSize - 1);
        int zDim = origZDim - (kernelSize - 1);
        int origSliceSize = origXDim * origYDim;
        int origVolume = origSliceSize * origZDim;
        double buffer[] = new double[origVolume];
        int x;
        int y;
        int z;
        int sliceSize = xDim * yDim;
        int volume = sliceSize * zDim;
        double gx[] = new double[volume];
        double gy[] = new double[volume];
        double gz[] = new double[volume];
        
        try {
            srcImage.exportData(0, origVolume, buffer);
        }
        catch (IOException e) {
                displayError("AlgorithmSobel: IOException on srcImage.exportData(0, origVolume, buffer)");

                setCompleted(false);

                return;
        }
    
        for (z = 1; z < origZDim - 1; z++) {
            fireProgressStateChanged((100 * (z-1))/zDim);
            for (y = 1; y < origYDim - 1; y++) {
                for (x = 1; x < origXDim - 1; x++) {
                    gx[x - 1 + (y - 1)*xDim + (z-1)*sliceSize] = 
                      buffer[x+1 + (y-1)*origXDim + (z-1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + (y-1)*origXDim + z*origSliceSize] - 2.0*buffer[x-1 + (y-1)*origXDim + z*origSliceSize]
                    + buffer[x+1 + (y-1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z+1)*origSliceSize]
                    + 2.0 * buffer[x+1 + y*origXDim + (z-1)*origSliceSize] - 2.0 * buffer[x-1 + y*origXDim + (z-1)*origSliceSize] 
                    + 4.0 * buffer[x+1 + y*origXDim + z*origSliceSize] - 4.0 * buffer[x-1 + y*origXDim + z*origSliceSize] 
                    + 2.0 * buffer[x+1 + y*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x-1 + y*origXDim + (z+1)*origSliceSize] 
                    + buffer[x+1 + (y+1)*origXDim + (z-1)*origSliceSize] - buffer[x-1 + (y+1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + (y+1)*origXDim + z*origSliceSize] - 2.0*buffer[x-1 + (y+1)*origXDim + z*origSliceSize]
                    + buffer[x+1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y+1)*origXDim + (z+1)*origSliceSize];
                    
                    gy[x - 1 + (y - 1)*xDim + (z-1)*sliceSize] = 
                      buffer[x-1 + (y+1)*origXDim + (z-1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x-1 + (y+1)*origXDim + z*origSliceSize] - 2.0 * buffer[x-1 + (y-1)*origXDim + z*origSliceSize] 
                    + buffer[x-1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z+1)*origSliceSize]
                    + 2.0 * buffer[x + (y+1)*origXDim + (z-1)*origSliceSize] - 2.0 * buffer[x + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 4.0 * buffer[x + (y+1)*origXDim + z*origSliceSize] - 4.0 * buffer[x + (y-1)*origXDim + z*origSliceSize] 
                    + 2.0 * buffer[x + (y+1)*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x + (y-1)*origXDim + (z+1)*origSliceSize]
                    + buffer[x+1 + (y+1)*origXDim + (z-1)*origSliceSize] - buffer[x+1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + (y+1)*origXDim + z*origSliceSize] - 2.0 * buffer[x+1 + (y-1)*origXDim + z*origSliceSize] 
                    + buffer[x+1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x+1 + (y-1)*origXDim + (z+1)*origSliceSize];
                    
                    gz[x - 1 + (y - 1)*xDim + (z-1)*sliceSize] = 
                      buffer[x-1 + (y-1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x-1 + y*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x-1 + y*origXDim + (z-1)*origSliceSize]
                    + buffer[x-1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y+1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x + (y-1)*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 4.0 * buffer[x + y*origXDim + (z+1)*origSliceSize] - 4.0 * buffer[x + y*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x + (y+1)*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x + (y+1)*origXDim + (z-1)*origSliceSize]
                    + buffer[x+1 + (y-1)*origXDim + (z+1)*origSliceSize] - buffer[x+1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + y*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x+1 + y*origXDim + (z-1)*origSliceSize]
                    + buffer[x+1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x+1 + (y+1)*origXDim + (z-1)*origSliceSize];
                } // for (x = 1; x < origXDim - 1; x++)
            } // for (y = 1; y < origYDim - 1; y++)
        } // for (z = 1; z < origZDim - 1; z++)
        
        try {
            destImage.importData(0, gx, false);
        }
        catch(IOException e) {
            displayError("AlgorithmSobel: IOException on destImage.importData(0, gx, false)");

            setCompleted(false);

            return;  
        }
        
        try {
            destImage.importData(volume, gy, false);
        }
        catch(IOException e) {
            displayError("AlgorithmSobel: IOException on destImage.importData(volume, gy, false)");

            setCompleted(false);

            return;  
        }
        
        try {
            destImage.importData(2*volume, gz, true);
        }
        catch(IOException e) {
            displayError("AlgorithmSobel: IOException on destImage.importData(2*volume, gz, true)");

            setCompleted(false);

            return;  
        }
        
        setCompleted(true);
        return;
        
    }
}