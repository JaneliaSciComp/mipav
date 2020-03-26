package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewOpenFileUI;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * This abstract class defines terms common to both T1 and T2 processing, such as thresholding and 
 * multithreading management methods.  Some methods were adapted from the ImageJ code by Sean Deoni.
 * 
 * @author senseneyj
 *
 */
public abstract class AlgorithmTProcess extends AlgorithmBase {
    
    public static final int MAX_PROCESS = 20;
    
    protected boolean upperLeftCorner = true;
    protected boolean upperRightCorner = false;
    protected boolean lowerLeftCorner = false;
    protected boolean lowerRightCorner = false;
    
    protected boolean useSmartThresholding = true;
    protected boolean useHardThresholding = false;
    protected float noiseScale = (float) 4.00;
    protected float hardNoiseThreshold = (float) 0.00;
    
    protected int processDataThreads;
    protected int computeDataThreads;
    protected int loadDataThreads;
    
    /** The number of slices in the largest volume */
    protected int nSlices;
    
    /**Whether this algorithm will utilize 4D processing optimization */
    protected boolean do4D;
    
    /** Static 4x4 Gaussian kernel */
    protected static double[][] Gaussian;
    
    public AlgorithmTProcess(ModelImage destImage, ModelImage srcImage) {
        super(destImage, srcImage);
        
        genericInit();
    }

    public AlgorithmTProcess() {
        super();
        
        genericInit();
    }

    /**
     * This method gives <code>clo</code> the file info of <code>orig</code>.
     * 
     * @param orig Original image
     * @param clo Near clone image
     */
    public void cloneFileInfo(ModelImage orig, ModelImage clo) {
        FileInfoBase[] oArr = orig.getFileInfo();  
        FileInfoBase[] cArr = new FileInfoBase[oArr.length];
        for(int i=0; i<oArr.length; i++) {
            cArr[i] = (FileInfoBase) oArr[i].clone();
        }
        
        clo.setFileInfo(cArr);

    }

    protected abstract void computeProcessors();

    protected abstract void displayImages();

    private void genericInit() {
    	Gaussian = new double[5][5];
        
        Gaussian[0][0] = 0;
        Gaussian[0][1] = 0;
        Gaussian[0][2] = 1;
        Gaussian[0][3] = 0;
        Gaussian[0][4] = 0;
        Gaussian[1][0] = 0;
        Gaussian[1][1] = 2;
        Gaussian[1][2] = 4;
        Gaussian[1][3] = 2;
        Gaussian[1][4] = 0;
        Gaussian[2][0] = 1;
        Gaussian[2][1] = 4;
        Gaussian[2][2] = 6;
        Gaussian[2][3] = 4;
        Gaussian[2][4] = 1;
        Gaussian[3][0] = 0;
        Gaussian[3][1] = 2;
        Gaussian[3][2] = 4;
        Gaussian[3][3] = 2;
        Gaussian[3][4] = 0;
        Gaussian[4][0] = 0;
        Gaussian[4][1] = 0;
        Gaussian[4][2] = 1;
        Gaussian[4][3] = 0;
        Gaussian[4][4] = 0;
	}
    
    protected abstract class CalculateT {

        protected int width;
        protected int height;
        
        protected int t;
        
        protected ViewJProgressBar dataBar;
        
        public CalculateT(int t) {
            this.dataBar = new ViewJProgressBar("computeData "+t, "Initialized...", 0, 100, false);
            dataBar.setVisible(false);
            this.t = t;
        }
        
        /**
         * Performs smart thresholding for the given image at at certain slice k
         * 
         * @param image
         * @param k
         * @return
         */
        protected float calculateThreshold(ModelImage image, int k) {
            float noiseSum = (float) 0.00, threshold = (float)0.00;
            int noiseIndex = 0;
            int x, y;
            if (upperLeftCorner) {
                for (y=20; y<30; y++) {
                    for (x=20; x<30; x++) {
                        if(image.getNDims() < 4) {
                            noiseSum += image.getFloat(x, y, k);
                        } else {
                            noiseSum += image.getFloat(x, y, k, t);
                        }
                        noiseIndex++;
                    }
                }
            }
            if (upperRightCorner) {
                for (y=20; y<30; y++) {
                    for (x=width-30; x<width-20; x++) {
                        if(image.getNDims() < 4) {
                            noiseSum += image.getFloat(x, y, k);
                        } else {
                            noiseSum += image.getFloat(x, y, k, t);
                        }
                    }
                }
            }
            if (lowerLeftCorner) {
                for (y=height-30; y<height-20; y++) {
                    for (x=20; x<30; x++) {
                        if(image.getNDims() < 4) {
                            noiseSum += image.getFloat(x, y, k);
                        } else {
                            noiseSum += image.getFloat(x, y, k, t);
                        }
                    }
                }
            }
            if (lowerRightCorner) {
                for (y=height-30; y<height-20; y++) {
                    for (x=width-30; x<width-20; x++) {
                        if(image.getNDims() < 4) {
                            noiseSum += image.getFloat(x, y, k);
                        } else {
                            noiseSum += image.getFloat(x, y, k, t);
                        }
                    }
                }
            }
            
            threshold = (float) ( (noiseSum/noiseIndex + 1)*noiseScale );
            return threshold;
        }

        protected abstract void initializeLocalImages(ModelImage image);
    }
}
