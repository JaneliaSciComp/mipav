package nibib.spim;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;



public class PlugInAlgorithmStageScan extends AlgorithmBase {
	
	private final Collection<ModelImage> resultImageList;
	
	private final String AFileDark2D;
	
	private final double ALeftShift;
	
	private File[][] AImageAr;
	
	private final String BFileDark2D;
	
	private final double BLeftShift;
	
	private File[][] BImageAr;
	
	private int subDirectoryLowerBound;
	
	private File resultDirectory;
	
	private final int concurrentNum;
	
	private Semaphore semaphore;
	
	private boolean bufferAvailable[];
	
	private boolean allowScriptRecording = false;

	
	public PlugInAlgorithmStageScan(final String AFileDark2D, final double ALeftShift,
			File[][] AImageAr, final String BFileDark2D, final double BLeftShift,
			File[][] BImageAr, int subDirectoryLowerBound,File resultDirectory, final int concurrentNum) {
		this.AFileDark2D = AFileDark2D;
		this.ALeftShift = ALeftShift;
		this.AImageAr = AImageAr;
		this.BFileDark2D = BFileDark2D;
		this.BLeftShift = BLeftShift;
		this.BImageAr = BImageAr;
		this.subDirectoryLowerBound = subDirectoryLowerBound;
		this.resultDirectory = resultDirectory;
		this.concurrentNum = concurrentNum;
		this.resultImageList = Collections.synchronizedCollection(new ArrayList<ModelImage>());
		
	}
	
	 // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    @Override
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm. At the conclusion of this method, AlgorithmBase reports to any algorithm listeners that
     * this algorithm has completed. This method is not usually called explicitly by a controlling dialog. Instead, see
     * AlgorithmBase.run() or start().
     */
    @Override
    public void runAlgorithm() {
        int i;
        int j;
        int k;
        int z;
        boolean found;
        int cIndex = 0;
        double leftShift;
        int subDirectoryNumber = AImageAr.length;
        int zDim = AImageAr[0].length;
        final boolean appFrameFlag = ViewUserInterface.getReference().isAppFrameVisible();
        ViewUserInterface.getReference().setAppFrameVisible(false);
        final FileIO io = new FileIO();
        io.setQuiet(true);
        io.setSuppressProgressBar(true);
        io.setTIFFOrientation(false);
        ModelImage ADarkImage = io.readImage(AFileDark2D);
        int extents[] = ADarkImage.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int sliceSize = xDim * yDim;
        float ADarkBuffer[][] = new float[concurrentNum][sliceSize];
        float BDarkBuffer[][] = new float[concurrentNum][sliceSize];
        try {
        	ADarkImage.exportData(0, sliceSize,ADarkBuffer[0]);
        }
        catch(IOException e) {
        	e.printStackTrace();
        	setCompleted(false);
        	return;
        }
        ADarkImage.disposeLocal();
        ADarkImage = null;
        for (i = 1; i < concurrentNum; i++) {
            for (j = 0; j < sliceSize; j++) {
                ADarkBuffer[i][j] = ADarkBuffer[0][j];
            }
        }
        
        ModelImage BDarkImage = io.readImage(BFileDark2D);
        try {
        	BDarkImage.exportData(0, sliceSize,BDarkBuffer[0]);
        }
        catch(IOException e) {
        	e.printStackTrace();
        	setCompleted(false);
        	return;
        }
        BDarkImage.disposeLocal();
        BDarkImage = null;
        for (i = 1; i < concurrentNum; i++) {
            for (j = 0; j < sliceSize; j++) {
                BDarkBuffer[i][j] = BDarkBuffer[0][j];
            }
        }
        
        bufferAvailable = new boolean[concurrentNum];
        for (i = 0; i < concurrentNum; i++) {
        	bufferAvailable[i] = true;
        }
        float resultBuffer[][] = new float[zDim][sliceSize];
        ThreadPoolExecutor exec;
        semaphore = new Semaphore(concurrentNum);
        
        int extents3D[] = new int[]{xDim, yDim, zDim};
        ModelImage AImage = new ModelImage(ModelStorageBase.FLOAT, extents3D, "AResultImage");
        int length = sliceSize * zDim;
        float rBuffer[] = new float[length];
        
        final FileWriteOptions options = new FileWriteOptions(null, null, true);
        options.setFileType(FileUtility.TIFF);
        options.setIsScript(true);
        options.setBeginSlice(0);
        options.setEndSlice(zDim - 1);
        options.setOptionsSet(true);
        String directory = resultDirectory.getAbsolutePath() + File.separatorChar;

        File file = new File(directory);

        if (!file.exists()) {
            file.mkdir();
        }
        
        String Adirectory = directory + "SPIMA" + File.separator;
        file = new File(Adirectory);

        if (!file.exists()) {
            file.mkdir();
        }
        
        ModelImage BImage = new ModelImage(ModelStorageBase.FLOAT, extents3D, "BResultImage");
        
        String Bdirectory = directory + "SPIMB" + File.separator;
        file = new File(Bdirectory);

        if (!file.exists()) {
            file.mkdir();
        }
        
        for (k = 0; k < subDirectoryNumber; k++) {
        	
        	exec = new ThreadPoolExecutor(concurrentNum, concurrentNum, 0L, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<Runnable>());
	        for (i = 0; i < zDim; i++) {
	        	try {
	                semaphore.acquire();
	            } catch (final InterruptedException e) {
	                System.out.println("Interrupted exception on semaphore.acquire() " + e);
	            }
	        	found = false;
	        	for (j = 0; j < concurrentNum && !found; j++) {
	        		if (bufferAvailable[j]) {
	        			bufferAvailable[j] = false;
	        			found = true;
	        			cIndex = j;
	        		}
	        	}
	        	leftShift = i * ALeftShift;
	        	final StageScanAlg algInstance = new StageScanAlg(cIndex, xDim, yDim, ADarkBuffer[cIndex], AImageAr[k][i], 
	        			resultBuffer[zDim - 1 - i], leftShift);
	            exec.execute(algInstance);
	        } // for (i = 0; i < zDim; i++)
       
        
	        exec.shutdown();
	
	        try {
	            exec.awaitTermination(1, TimeUnit.DAYS);
	        } catch (final InterruptedException e) {
	            MipavUtil.displayError("Program did not execute correctly");
	            e.printStackTrace();
	        }
	        
	        
	        for (z = 0; z < zDim; z++) {
	            for (i = 0; i < sliceSize; i++) {
	            	rBuffer[z*sliceSize + i] = resultBuffer[z][i];
	            }
	        }
	        try {
	        	AImage.importData(0, rBuffer, true);
	        }
	        catch(IOException e) {
	        	e.printStackTrace();
	        	setCompleted(false);
	        	return;
	        }
	        
	        
	        options.setFileDirectory(Adirectory);
	        String name = AImageAr[0][0].getName();
	        int upper = -1;
	        found = false;
	        for(i=name.length()-1; i >= 0 && upper == -1; i--) {
	            if( (Character.isDigit(name.charAt(i))) || (name.substring(i,i+1).equals("_"))) {
	            	found = true;
	            }
	            else if (found){
	            	upper = i+1;
	            }
	        }
	        name = name.substring(0,upper) + "_" + String.valueOf(k + subDirectoryLowerBound);
	        options.setFileName(name);
	        io.writeImage(AImage, options, false, allowScriptRecording);
	        
	        for (i = 0; i < concurrentNum; i++) {
	        	bufferAvailable[i] = true;
	        }
	        exec = new ThreadPoolExecutor(concurrentNum, concurrentNum, 0L, TimeUnit.MILLISECONDS,
	                new LinkedBlockingQueue<Runnable>());
	        
	        for (i = 0; i < zDim; i++) {
	        	try {
	                semaphore.acquire();
	            } catch (final InterruptedException e) {
	                System.out.println("Interrupted exception on semaphore.acquire() " + e);
	            }
	        	found = false;
	        	for (j = 0; j < concurrentNum && !found; j++) {
	        		if (bufferAvailable[j]) {
	        			bufferAvailable[j] = false;
	        			found = true;
	        			cIndex = j;
	        		}
	        	}
	        	leftShift = i * BLeftShift;
	        	final StageScanAlg algInstance = new StageScanAlg(cIndex, xDim, yDim, BDarkBuffer[cIndex], BImageAr[k][i], 
	        			resultBuffer[i], leftShift);
	            exec.execute(algInstance);
	        } // for (i = 0; i < zDim; i++)
	        
	        exec.shutdown();
	
	        try {
	            exec.awaitTermination(1, TimeUnit.DAYS);
	        } catch (final InterruptedException e) {
	            MipavUtil.displayError("Program did not execute correctly");
	            e.printStackTrace();
	        }
	        
	        for (z = 0; z < zDim; z++) {
	            for (i = 0; i < sliceSize; i++) {
	            	rBuffer[z*sliceSize + i] = resultBuffer[z][i];
	            }
	        }
	        try {
	        	BImage.importData(0, rBuffer, true);
	        }
	        catch(IOException e) {
	        	e.printStackTrace();
	        	setCompleted(false);
	        	return;
	        }
	        
	        options.setFileDirectory(Bdirectory);
	        name = BImageAr[0][0].getName();
	        upper = -1;
	        found = false;
	        for(i=name.length()-1; i >= 0 && upper == -1; i--) {
	            if( (Character.isDigit(name.charAt(i))) || (name.substring(i,i+1).equals("_"))) {
	            	found = true;
	            }
	            else if (found) {
	            	upper = i+1;
	            }
	        }
	        name = name.substring(0,upper) + "_" + String.valueOf(k + subDirectoryLowerBound);
	        options.setFileName(name);
	        io.writeImage(BImage, options, false, allowScriptRecording);
        } // for (k = 0; k < subDirectoryNumber; k++)
        AImage.disposeLocal();
        AImage = null;
        BImage.disposeLocal();
        BImage = null;
        ViewUserInterface.getReference().setAppFrameVisible(appFrameFlag);
        setCompleted(true);
        return;
    }
    
    public class StageScanAlg implements Runnable {
    	private int cIndex;
    	private int xDim;
    	private int yDim;
    	private float darkBuffer[];
    	private File imageFile;
    	private float resultBuffer[];
    	private double leftShift;
    	
    	public StageScanAlg(int cIndex, int xDim, int yDim,
    			float darkBuffer[], File imageFile, float resultBuffer[], double leftShift) {
    	    this.cIndex = cIndex;
    	    this.xDim = xDim;
    	    this.yDim = yDim;
    	    this.darkBuffer = darkBuffer;
    	    this.imageFile = imageFile;
    	    this.resultBuffer = resultBuffer;
    	    this.leftShift = leftShift;
    	}
    	
    	@Override
        public void run() {
            call();
            bufferAvailable[cIndex] = true;
            semaphore.release();
        }

        public Boolean call() {
        	int i;
        	int x;
        	int y;
        	int shift;
        	int upperXShift;
        	int lowerShift;
            int upperShift;
            float lowerFraction;
            float upperFraction;
        	int ypos;
        	final FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            io.setTIFFOrientation(false);
            ModelImage image = io.readImage(imageFile.getAbsolutePath());
            int sliceSize = darkBuffer.length;
            try {
            	image.exportData(0, sliceSize, resultBuffer);
            }
            catch(IOException e) {
            	e.printStackTrace();
            	return false;
            }
            image.disposeLocal();
            image = null;
            for (i = 0; i < sliceSize; i++) {
            	resultBuffer[i] -= darkBuffer[i];
            }
            if (Math.round(leftShift) == leftShift) {
            	shift = (int)leftShift;
            	upperXShift = xDim - 1 - shift;
                for (y = 0; y < yDim; y++) {
                	ypos = y * xDim;
            	    for (x = 0; x <= upperXShift; x++) {
            	    	resultBuffer[x + ypos] = resultBuffer[x + shift + ypos];
            	    }
            	    for (x = upperXShift + 1; x <= xDim-1; x++) {
            	    	resultBuffer[x + ypos] = 0.0f;
            	    }
                } // for (y = 0; y < yDim; y++)
            } // if (Math.round(leftShift) == leftShift)
            else {
            	lowerShift = (int)leftShift;
                upperShift = lowerShift + 1;
                upperXShift = xDim - 1 - upperShift;
                lowerFraction = (float)(Math.ceil(leftShift) - leftShift);
                upperFraction = 1.0f - lowerFraction;
                for (y = 0; y < yDim; y++) {
                    ypos = y * xDim;
                    for (x = 0; x <= upperXShift; x++) {
                    	resultBuffer[x + ypos] = lowerFraction * resultBuffer[x + lowerShift + ypos] +
                    			                 upperFraction * resultBuffer[x + upperShift + ypos];
                    }
                    for (x = upperXShift + 1; x <= xDim-1; x++) {
            	    	resultBuffer[x + ypos] = 0.0f;
            	    }
                } // for (y = 0; y < yDim; y++)
            }
            return true;
        }
    }
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }
}