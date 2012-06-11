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
    
    /**This object processes data for the tre code */
    protected DataListener dataListener;
    
    protected String tempDirString;
    
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
     * This method gives <code>clo</code> most of the image attributes of <code>orig</code> besides
     * data type to minimize rounding errors.
     * 
     * @param orig Original image
     * @param clo Near clone image
     */
    public ModelImage nearCloneImage(ModelImage orig, ModelImage clo) {
        FileInfoBase[] oArr = orig.getFileInfo();
        FileInfoBase[] cArr = clo.getFileInfo();
        if(cArr.length != oArr.length) {
            MipavUtil.displayError("Images are not same length");
            return clo;
        }
        for(int i=0; i<cArr.length; i++) {
            if(cArr == null) {
                return clo;
            }
            cArr[i].setOffset(oArr[i].getOffset());
            cArr[i].setEndianess(oArr[i].getEndianess());
            cArr[i].setResolutions(oArr[i].getResolutions().clone());
            cArr[i].setUnitsOfMeasure(oArr[i].getUnitsOfMeasure().clone());
            cArr[i].setOrigin(oArr[i].getOrigin().clone());
            cArr[i].setImageOrientation(cArr[i].getImageOrientation());
            cArr[i].setAxisOrientation(oArr[i].getAxisOrientation().clone());
            cArr[i].setDataType(ModelImage.DOUBLE);
        }
        
        return clo;
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

	/**
     * This method saves an image volume into temporary storage for later loading
     * by <code>loadResultData()</code>
     * 
     * @param tempVolume
     * @param volumeNumber
     * @return whether the temp file directory is writable
     */
    private boolean saveImageData(ModelImage tempVolume, int volumeNumber) {
        ViewUserInterface.getReference().getMessageFrame().append("Saving to: "+tempDirString+tempVolume.getImageName(), ViewJFrameMessage.DEBUG);
        FileWriteOptions options = new FileWriteOptions(false);

        options.setBeginSlice(0);
        options.setEndSlice(tempVolume.getExtents()[2] - 1);
        System.out.println("Running in sep thread: "+runningInSeparateThread);
        options.setRunningInSeparateThread(runningInSeparateThread);
        options.setFileType(FileUtility.RAW);
        options.setFileName(tempVolume.getImageName());
        options.setFileDirectory(tempDirString);
        options.setIsScript(!runningInSeparateThread);
        options.setOptionsSet(true); // Options have been set - therefore don't bring up any dialogs
        
        FileIO fileIO = new FileIO();
        fileIO.writeImage(tempVolume, options, false);
        
        return true;
    }
    
    /**
     * When system permissions do not allow image data to be saved to a computer, this method
     * performs the older, slower process of storing image data locally in memory.  This method
     * is also the last step of creating the t1, m0, r1, and b1 result stacks.
     * 
     * @param tempVolume
     * @param volumeNumber
     */
    private void storeImageData(ModelImage resultStack, ModelImage tempVolume, int volumeNumber) {
        try {
            int startVal = resultStack.getSliceSize()*nSlices*volumeNumber;
            int sliceSize = tempVolume.getSliceSize();
            float[] volumeData = new float[nSlices*sliceSize];
            tempVolume.exportData(0, sliceSize*nSlices, volumeData);
            resultStack.importData(startVal, volumeData, true);
        } catch(IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("Could not import result image data.");
        }
        
        return;
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

        protected abstract void disposeLocal();

        protected abstract void initializeLocalImages(ModelImage image);
    }
    
    protected class DataListener extends Thread {
    
        private Queue<ImageHolder> dataImport;
        private ViewJProgressBar[] dataBar;
        private boolean localInterrupt;
        private int currentBar;
        private ArrayList<Future<?>> workList;
        
        public DataListener() {
            dataImport = new ConcurrentLinkedQueue<ImageHolder>();
            dataBar = new ViewJProgressBar[processDataThreads];
            for(int i=0; i<dataBar.length; i++) {
                dataBar[i] = new ViewJProgressBar("Data importer "+i, "Initialized...", 0, 100, false);
                dataBar[i].setVisible(true);
            }
            localInterrupt = false;
            currentBar = 0;
            workList = new ArrayList<Future<?>>();
        }
        
        public boolean insertData(ModelImage image, float[][] data, int time) {
            dataBar[0].setMessage("Receiving data from series "+time);
            ImageHolder h = new ImageHolder(image, data, time);
            return dataImport.offer(h);
        }
        
        public boolean isLocalInterrupted() {
            return localInterrupt;
        }

        public void localInterrupt() {
            localInterrupt = true;
        }

        public void run() {
            ExecutorService exec = Executors.newFixedThreadPool(processDataThreads);
            ProcessImageHolder b;
            Future<?> f;
            boolean firstTime = true;
            while(firstTime || !allComplete()) {
                firstTime = false;
                while(!isLocalInterrupted()) {
                    while(!dataImport.isEmpty()) {
                        b = new ProcessImageHolder(dataImport.poll(), currentBar);
                        f = exec.submit(b);
                        currentBar++;
                        if(currentBar > dataBar.length-1) {
                            currentBar = 0;
                        }
                        workList.add(f);
                    }
                }
            }
            
            exec.shutdown();
            
            for(int i=0; i<dataBar.length; i++) {
                dataBar[i].setVisible(false);
                //dataBar[i].dispose();
                dataBar[i] = null;
            }
            
            dataBar = null;
        }
        
        private boolean allComplete() {
            for(int i=0; i<workList.size(); i++) {
                if(workList.get(i).isDone()) {
                    workList.remove(i);
                    i = i==0 ? 0 : i-1;  
                }
            }
            
            if(workList.size() > 0) {
                return false;
            } else {
                return true;
            }
        }

        private class ProcessImageHolder implements Runnable {
    
            private ImageHolder h;
            private ModelImage image;
            private float[][] data;
            private int t, index;
    
            public ProcessImageHolder(ImageHolder h, int index) {
                this.h = h;
                this.index = index;
            }
            
            public void run() {
                if(h != null) {
                    dataBar[index].setVisible(true);
                    image = h.getImage();
                    data = h.getData();
                    t = h.getTime();
                    if(image.getImageName().equals("Unknown")) {
                        System.out.println("STOP");
                    }
                    ViewUserInterface.getReference().getMessageFrame().append("Working on: "+image.getImageName()+"\n", ViewJFrameMessage.DEBUG);
                    String imageName = image.getImageName().substring(0, image.getImageName().length() > 5 ? 4 : image.getImageName().length()); 
                    int startVal = 0;
                    for(int k=0; k<nSlices; k++) {
                        try {
                            dataBar[index].setMessage("Importing "+imageName+" volume: "+t+" slice: "+k);
                            startVal = image.getSliceSize()*k;
                            image.importData(startVal, data[k], true);
                        } catch (IOException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Could not import result image data.");
                        }
                        dataBar[index].updateValue((int)(((double)k/(nSlices+1))*80));
                    }
                    saveImageData(image, t);
                    image.disposeLocal();
                    image = null;
                    h = null;
                    dataBar[index].setVisible(false);
                } 
            }
        }
        
        private class ImageHolder {
            
            private ModelImage image;
            private float[][] data;
            private int time;
            
            public ImageHolder(ModelImage image, float[][] data, int time) {
                this.image = image;
                this.data = data;
                this.time = time;
            }
    
            public float[][] getData() {
                return data;
            }

            public ModelImage getImage() {
                return image;
            }
    
            public int getTime() {
                return time;
            }
            
        }
    }

    /**
     * This class loads all saved data that resulted from 4D processing in either the conventional or HIFI case
     */
    protected class LoadResultDataOuter implements Runnable {
        
        private ModelImage resultStack;
        private String imageName;
        private int tVolumes;

        public LoadResultDataOuter(ModelImage resultStack, String imageName, int tVolumes) {
            this.resultStack = resultStack;
            this.imageName = imageName;
            this.tVolumes = tVolumes;
        }

        public void run() {
            ExecutorService exec = Executors.newFixedThreadPool(loadDataThreads);
            ArrayList<LoadResultDataInner> f = new ArrayList<LoadResultDataInner>();
            LoadResultDataInner d;
            
            for(int i=0; i<tVolumes; i++) {
                fireProgressStateChanged("Reading image: "+imageName+", "+i);
                ViewUserInterface.getReference().getMessageFrame().append("Reading from: "+tempDirString+imageName+i+"\n", ViewJFrameMessage.DEBUG);
                d = new LoadResultDataInner(i, imageName, resultStack);
                f.add(d);
                exec.submit(d);
            }
            
            exec.shutdown();
            
            try {
                exec.awaitTermination(1, TimeUnit.DAYS);
            } catch(InterruptedException e) {
                MipavUtil.displayError("Program did not execute correctly");
                e.printStackTrace();
            }
        }
    }
    
    private class LoadResultDataInner implements Runnable {
        
        private final ViewOpenFileUI openFile = new ViewOpenFileUI(false);
        private int i;
        private String imageName;
        private ModelImage resultStack;
        
        public LoadResultDataInner(int i, String imageName, ModelImage resultStack) {
            this.i = i;
            this.imageName = imageName;
            this.resultStack = resultStack;
            
            openFile.setPutInFrame(false);
        }

        public void run() {
            
            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            ModelImage tempVolume = fileIO.readImage(imageName+i+".xml", tempDirString, false, null);
            
            if(tempVolume != null) {
                storeImageData(resultStack, tempVolume, i);
                tempVolume.disposeLocal();
                File f = new File(tempDirString+imageName+i+".raw");
                if(f != null) {
                    if(f.delete()) {
                        ViewUserInterface.getReference().getMessageFrame().append(f.getName()+" succeessfully deleted."+"\n", ViewJFrameMessage.DEBUG);
                    } else {
                        ViewUserInterface.getReference().getMessageFrame().append(f.getName()+" could not be deleted from your system, please delete manually."+"\n", ViewJFrameMessage.DEBUG);
                    }
                }
                f = new File(tempDirString+imageName+i+".xml");
                if(f != null) {
                    if(f.delete()) {
                        ViewUserInterface.getReference().getMessageFrame().append(f.getName()+" succeessfully deleted."+"\n", ViewJFrameMessage.DEBUG);
                    } else {
                        ViewUserInterface.getReference().getMessageFrame().append(f.getName()+" could not be deleted from your system, please delete manually."+"\n", ViewJFrameMessage.DEBUG);
                    }
                }
            }
        }
    }
}
