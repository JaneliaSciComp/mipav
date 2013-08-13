package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmThinning2D extends AlgorithmBase {
    // This is an implementation of the Section 9.5.5 Thinning in Digital Image Processing Third Edition
    // by Rafael C. Gonzalez and Richard E. Woods
    // The selfTest uses the example in Figure 9.21.
    
    /**
     * AlgorithmThinning2D - default constructor.
     */
    public AlgorithmThinning2D() { }
    
    /**
     * 
     * @param destImg
     * @param srcImg
     */
    public AlgorithmThinning2D(ModelImage destImg, ModelImage srcImg) {
          super(destImg, srcImg);
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int sliceSize;
        double buffer[];
        double paddedBuffer[];
        double processed[];
        double temp[];
        boolean change;
        int element;
        int x;
        int y;
        int paddedXDim;
        int paddedYDim;
        int paddedSliceSize;
        boolean selfTest = false;
        double testBuffer[] = null;
        int i;
        int errors;
        
        if (selfTest) {
            xDim = 11;
            yDim = 5;
            int extents[] = new int[2];
            extents[0] = xDim;
            extents[1] = yDim;
            sliceSize = xDim * yDim;
            buffer = new double[sliceSize];
            for (i = 0; i < sliceSize; i++) {
                buffer[i] = 1.0;
            }
            for (y = 1; y < yDim; y++) {
                for (x = 9; x < xDim; x++) {
                    buffer[x + y * xDim] = 0.0;
                }
            }
            buffer[3 + (yDim-1)*xDim] = 0.0;
            buffer[4 + (yDim-1)*xDim] = 0.0;
            
            testBuffer = new double[sliceSize];
            testBuffer[0] = 1.0;
            testBuffer[8] = 1.0;
            testBuffer[9] = 1.0;
            testBuffer[10] = 1.0;
            testBuffer[11] = 1.0;
            testBuffer[12] = 1.0;
            testBuffer[18] = 1.0;
            testBuffer[19] = 1.0;
            for (i = 23; i <= 29; i++) {
                testBuffer[i] = 1.0;
            }
            testBuffer[33] = 1.0;
            testBuffer[34] = 1.0;
            testBuffer[40] = 1.0;
            testBuffer[44] = 1.0;
            testBuffer[51] = 1.0;
            testBuffer[52] = 1.0;
        } // if (selfTest)
        else {
        
        
            if (srcImage == null) {
                displayError("Source Image is null");
                finalize();
    
                return;
            }
            
            fireProgressStateChanged(srcImage.getImageName(), "Running Thinning ...");
    
            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
            sliceSize = xDim * yDim;
            buffer = new double[sliceSize];
            try {
                srcImage.exportData(0, sliceSize, buffer);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOExeption " + e + " on srcImage.exportData(0, sliceSize, buffer)");
                setCompleted(false);
                return;
            }
        }
        
        paddedXDim = xDim + 2;
        paddedYDim = yDim + 2;
        paddedSliceSize = paddedXDim * paddedYDim;
        paddedBuffer = new double[paddedSliceSize];
        processed = new double[paddedSliceSize];
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                paddedBuffer[x + 1 + (y + 1) * paddedXDim] = buffer[x + y * xDim];
            }
        }
        
        
        change = true;
        while (change) {
            change = false;
            for (element = 1; element <= 8; element++) {
                for (y = 1; y <= yDim; y++) {
                    for (x = 1; x <= xDim; x++) {
                        switch (element) {
                            case 1:
                                if ((paddedBuffer[x-1 + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x+1 + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + y * paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + (y+1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + (y+1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + (y+1)*paddedXDim] != 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 2:
                                if ((paddedBuffer[x + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x+1 + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x-1 + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + y*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x-1 + (y+1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + (y+1)*paddedXDim] != 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 3:
                                if ((paddedBuffer[x-1 + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x-1 + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + y*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x-1 + (y+1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + (y+1)*paddedXDim] == 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 4:
                                if ((paddedBuffer[x-1 + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + y*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + (y+1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x+1 + (y+1)*paddedXDim] == 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 5:
                                if ((paddedBuffer[x-1 + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + (y+1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + (y+1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x+1 + (y+1)*paddedXDim] == 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 6:
                                if ((paddedBuffer[x + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + y*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + (y+1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + (y+1)*paddedXDim] == 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 7:
                                if ((paddedBuffer[x-1 + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x+1 + (y-1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + y*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x-1 + (y+1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x+1 + (y+1)*paddedXDim] != 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                                break;
                            case 8:
                                if ((paddedBuffer[x-1 + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + (y-1)*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x-1 + y*paddedXDim] == 0.0) &&
                                    (paddedBuffer[x + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + y*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x + (y+1)*paddedXDim] != 0.0) &&
                                    (paddedBuffer[x+1 + (y+1)*paddedXDim] != 0.0)) {
                                    processed[x + y * paddedXDim] = 0.0;
                                    change = true;
                                }
                                else {
                                    processed[x + y * paddedXDim] = paddedBuffer[x + y * paddedXDim];
                                }
                        } // switch (element)
                    } // for (x = 1; x <= xDim-2; x++)
                } // for (y = 1; y <= yDim-2; y++)
                if ((element < 8) || change) {
                    temp = paddedBuffer;
                    paddedBuffer = processed;
                    processed = temp;
                } // if ((element < 8) || change)
            } // for (element = 1; element <= 8; element++)
        } // while (change)
        
        paddedBuffer = null;
        
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                buffer[x + y * xDim] = processed[x + 1 + (y + 1) * paddedXDim];
            }
        }
        
        processed = null;
        
        if (selfTest) {
            errors = 0;
            for (i = 0; i < sliceSize; i++) {
                if (buffer[i] != testBuffer[i]) {
                    System.out.println("buffer["+i+"] = " + buffer[i] + " != testBuffer[" + i + "] = " + testBuffer[i]);
                    errors++;
                }
            }
            System.out.println("Errors = " + errors);
            
            setCompleted(true);
            return;
        } // if (selfTest)
        
        if (destImage != null) {
            try {
                destImage.importData(0, buffer, true);
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException " + e + " on destImage.importData(0, buffer, true)");
                setCompleted(false);
                return;
            }
        } // if (destImage != null)
        else {
            try {
                srcImage.importData(0, buffer, true);
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.importData(0, buffer, true)");
                setCompleted(false);
                return;
            }    
        } // else
        buffer = null;
        
        setCompleted(true);
        return;
    }
}