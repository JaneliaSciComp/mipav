package nibib.spim;
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.util.ThreadUtil;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmDeconvolution;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogScriptableTransform;

/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. 
 * By extending AlgorithmBase, it has no more functionality than any other algorithm in MIPAV.
 * No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInAlgorithmGenerateFusion extends AlgorithmBase {

    public enum SampleMode {
        UpsampleToTransform("Upsample base image to transformed"),
        DownsampleToBase("Downsample transformed image to base"),
        DownsampleUpsampleCombined("Do both sampling mechanisms");
        
        private String str;

        SampleMode(String str) {
            this.str = str;
        }

        public String toString() {
            return str;
        }
    }
    
    private boolean register = true;
    private boolean showAriMean = true; 
    private boolean doShowPrefusion = false;
    private boolean doInterImages = false;
    private boolean showGeoMean = false;
    private File[] baseImageAr;
    private File[] transformImageAr;
    private boolean doThreshold;
    private double resX = 1;
    private double resY = 1;
    private double resZ = 1;
    private double thresholdIntensity;
    private String mtxFileLoc;
    private String mtxFileDirectory;
    private int timeIndex;
    private int concurrentNum;
    private Integer xMovement;
    private Integer yMovement;
    private Integer zMovement;
    private Collection<ModelImage> resultImageList;
    private SampleMode mode;
    
    private final int stepSize;
    private final int minX, minY, minZ;
    private final int maxX, maxY, maxZ;
    
    private volatile ExecutorService exec = null;
    private boolean saveAriMean, saveGeoMean;
    private File ariMeanDir, geoMeanDir;
    private boolean doSavePrefusion;
    private File prefusionBaseDir, prefusionTransformDir;
    /**Weights to use for calculating arithmetic image means */
    private double baseAriWeight, transformAriWeight;
    /** Weights to use for calculating geometric image means */
    private double baseGeoWeight, transformGeoWeight;
    /** Whether maximum projections are shown and saved */
    private boolean showMaxProj, saveMaxProj;
    /** Optional MIP algorithm */
    private AlgorithmMaximumIntensityProjection[] maxAlgo;
    /** File format for saving result images */
	private String saveType;
	
	private boolean doDeconv;
	private boolean deconvShowResults;
	private int deconvIterations;
	private float[] deconvSigmaA, deconvSigmaB;
	private boolean useDeconvSigmaConversionFactor;
	private File deconvDir;
	private float rotateBeginX = -5.0f;
    private float rotateEndX = 5.0f;
    private float coarseRateX = 3.0f;
    private float fineRateX = 1.0f;
    private float rotateBeginY = -5.0f;
    private float rotateEndY = 5.0f;
    private float coarseRateY = 3.0f;
    private float fineRateY = 1.0f;
    private float rotateBeginZ = -5.0f;
    private float rotateEndZ = 5.0f;
    private float coarseRateZ = 3.0f;
    private float fineRateZ = 1.0f;
    private Semaphore semaphore;

    /**
     * Constructor.
     * @param register
     * @param rotateBeginX
     * @param rotateEndX
     * @param coarseRateX
     * @param fineRateX
     * @param rotateBeginY
     * @param rotateEndY
     * @param coarseRateY
     * @param fineRateY
     * @param rotateBeginZ
     * @param rotateEndZ
     * @param coarseRateZ
     * @param fineRateZ
     * @param doGeoMean 
     * @param doInterImages 
     * @param doShowPrefusion 
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     * @param scale 
     * @param image2Intensity 
     * @param doShowPrefusion 
     * @param middleSlice2 
     * @param thresholdIntensity 
     * @param resZ 
     * @param resY 
     * @param resX 
     * @param doThreshold 
     * @param doThreshold 
     * @param mtxFileLoc 
     * @param mtxFileDirectory
     * @param timeIndex
     * @param zMovement 
     * @param yMovement 
     * @param xMovement 
     * @param mode 
     * @param ariMeanDir 
     * @param saveAriMean 
     * @param geoMeanDir 
     * @param saveGeoMean 
     * @param prefusionTranformDir 
     * @param prefusionBaseDir 
     * @param doSavePrefusion 
     * @param transformAriWeight 
     * @param baseAriWeight 
     * @param transformGeoWeight 
     * @param baseGeoWeight 
     * @param maxAlgo can be null if no MIP is supposed to take place
     * @param saveType 
     * @param doDeconv
     * @param deconvIterations
     * @param deconvSigmaA
     * @param deconvSigmaB
     * @param useDeconvSigmaConversionFactor
     * @param deconvDir
     * @param deconvShowResults
     */
    public PlugInAlgorithmGenerateFusion(boolean register, float rotateBeginX, float rotateEndX, float coarseRateX, float fineRateX, 
                                         float rotateBeginY,  float rotateEndY, float coarseRateY, float fineRateY, float rotateBeginZ, 
                                         float rotateEndZ, float coarseRateZ, float fineRateZ,
                                                    boolean doShowPrefusion, boolean doInterImages, boolean doGeoMean, 
                                                    boolean doAriMean, boolean showMaxProj, 
                                                    boolean doThreshold, double resX, double resY, double resZ, int concurrentNum, double thresholdIntensity, String mtxFileLoc, 
                                                    String mtxFileDirectory, int timeIndex, File[] baseImageAr, File[] transformImageAr, 
                                                    Integer xMovement, Integer yMovement, Integer zMovement, SampleMode mode,
                                                    int minX, int minY, int minZ, int maxX, int maxY, int maxZ, int stepSize, 
                                                    boolean saveMaxProj, boolean saveGeoMean, File geoMeanDir, boolean saveAriMean, File ariMeanDir, 
                                                    boolean doSavePrefusion, File prefusionBaseDir, File prefusionTransformDir, 
                                                    double baseAriWeight, double transformAriWeight, double baseGeoWeight, double transformGeoWeight, AlgorithmMaximumIntensityProjection[] maxAlgo, String saveType,
                                                    boolean doDeconv, int deconvIterations, float[] deconvSigmaA, float[] deconvSigmaB, boolean useDeconvSigmaConversionFactor, File deconvDir, boolean deconvShowResults) {
        this.register = register;
        this.rotateBeginX = rotateBeginX;
        this.rotateEndX = rotateEndX;
        this.coarseRateX = coarseRateX;
        this.fineRateX = fineRateX;
        this.rotateBeginY = rotateBeginY;
        this.rotateEndY = rotateEndY;
        this.coarseRateY = coarseRateY;
        this.fineRateY = fineRateY;
        this.rotateBeginZ = rotateBeginZ;
        this.rotateEndZ = rotateEndZ;
        this.coarseRateZ = coarseRateZ;
        this.fineRateZ = fineRateZ;
        this.showAriMean = doAriMean;
        this.doShowPrefusion = doShowPrefusion;
        this.doInterImages = doInterImages;
        this.showGeoMean = doGeoMean;
        this.showMaxProj = showMaxProj;
        this.doThreshold = doThreshold;
        this.saveType = saveType;
        
        this.resX = resX;
        this.resY = resY;
        this.resZ = resZ;
        this.xMovement = xMovement;
        this.yMovement = yMovement;
        this.zMovement = zMovement;
        this.thresholdIntensity = thresholdIntensity;
        
        this.minX = minX;
        this.minY = minY;
        this.minZ = minZ;
        
        this.maxX = maxX;
        this.maxY = maxY;
        this.maxZ = maxZ;
        
        this.stepSize = stepSize;
        
        this.mtxFileLoc = mtxFileLoc;
        this.mtxFileDirectory = mtxFileDirectory;
        this.timeIndex = timeIndex;
        this.baseImageAr = baseImageAr;
        this.transformImageAr = transformImageAr;
        
        this.concurrentNum = concurrentNum;
        
        this.mode = mode;
        
        this.resultImageList = Collections.synchronizedCollection(new ArrayList<ModelImage>());
        
        this.saveGeoMean = saveGeoMean;
        this.geoMeanDir = geoMeanDir;
        this.saveAriMean = saveAriMean;
        this.ariMeanDir = ariMeanDir;
        this.saveMaxProj = saveMaxProj;
        
        
        this.doSavePrefusion = doSavePrefusion;
        this.prefusionBaseDir = prefusionBaseDir;
        this.prefusionTransformDir = prefusionTransformDir;
        
        this.baseAriWeight = baseAriWeight;
        this.transformAriWeight = transformAriWeight;
        
        this.baseGeoWeight = baseGeoWeight;
        this.transformGeoWeight = transformGeoWeight;
        
        this.maxAlgo = maxAlgo;
        
        this.doDeconv = doDeconv;
        this.deconvIterations = deconvIterations;
        this.deconvSigmaA = deconvSigmaA;
        this.deconvSigmaB = deconvSigmaB;
        this.useDeconvSigmaConversionFactor = useDeconvSigmaConversionFactor;
        this.deconvDir = deconvDir;
        this.deconvShowResults = deconvShowResults;
    }
        
    //  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    /**
     * Starts the algorithm.  At the conclusion of this method, AlgorithmBase reports to any
     * algorithm listeners that this algorithm has completed.  This method is not usually called explicitly by
     * a controlling dialog.  Instead, see AlgorithmBase.run() or start().
     */
    public void runAlgorithm() {
        
    	boolean appFrameFlag = ViewUserInterface.getReference().isAppFrameVisible();
    	ViewUserInterface.getReference().setAppFrameVisible(false);
    	
    	if (register) {
            AlgorithmRegOAR3D regAlgo3D = null;
            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            io.setTIFFOrientation(false);
            ModelImage baseImage = io.readImage(baseImageAr[timeIndex].getAbsolutePath());
            for (int i = 0; i < baseImage.getExtents()[2]; i++) {
                for (int j = 0; j < 3; j++) {
                    baseImage.getFileInfo(i).setUnitsOfMeasure(FileInfoBase.MICROMETERS, j);
                }
            }
            String baseImageName = baseImage.getImageName();
            ModelImage transformImage = io.readImage(transformImageAr[timeIndex].getAbsolutePath());
            for (int i = 0; i < transformImage.getExtents()[2]; i++) {
                for (int j = 0; j < 3; j++) {
                    transformImage.getFileInfo(i).setUnitsOfMeasure(FileInfoBase.MICROMETERS, j);
                }
            }
            String transformImageName = transformImage.getImageName();
            baseImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            transformImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            
            for(int i=0; i<baseImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(baseImage.getResolutions(i)[2]);
            }
            
            for(int i=0; i<transformImage.getFileInfo().length; i++) {
                transformImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            AlgorithmRotate rotate = new AlgorithmRotate(transformImage, AlgorithmRotate.Y_AXIS_MINUS);
            rotate.run(); //transform image replaced
            ViewUserInterface.getReference().unRegisterImage(transformImage);
            transformImage.disposeLocal();
            transformImage = rotate.getDestImage();
            rotate.finalize();
            int cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
            int DOF = 12;
            int interp = AlgorithmTransform.TRILINEAR;
            boolean maxResol = true;
            boolean doSubsample = true;
            boolean doMultiThread = Preferences.isMultiThreadingEnabled()  &&
                    (ThreadUtil.getAvailableCores() > 1);
            boolean fastMode = false;
            int maxIterations = 2;
            int numMinima = 3;
            regAlgo3D = new AlgorithmRegOAR3D(baseImage, transformImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                    coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                    rotateEndZ, coarseRateZ, fineRateZ, maxResol, doSubsample, doMultiThread, fastMode,
                    maxIterations, numMinima);
            regAlgo3D.run();
            if (regAlgo3D.isCompleted()) {
                int xdimA = baseImage.getExtents()[0];
                int ydimA = baseImage.getExtents()[1];
                int zdimA = baseImage.getExtents()[2];
                float xresA = baseImage.getFileInfo(0).getResolutions()[0];
                float yresA = baseImage.getFileInfo(0).getResolutions()[1];
                float zresA = baseImage.getFileInfo(0).getResolutions()[2];
                TransMatrix finalMatrix = regAlgo3D.getTransform();
                // Do not do finalMatrix.Inverse()
                finalMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                String costName = "CORRELATION_RATIO_SMOOTHED";
                String message = "Using cost function, " + costName;
                message += ", the cost is " + Double.toString(regAlgo3D.getAnswer()) + ".\n";
                message += "Some registration settings: \n";
                message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
                message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
                message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
                mtxFileLoc = mtxFileDirectory + File.separator + transformImageName + "_To_"
                        + baseImageName + ".mtx";
                int interp2 = AlgorithmTransform.TRILINEAR;
                boolean pad = false;
                System.out.println("finalMatrix = " + finalMatrix);
                finalMatrix.saveMatrix(mtxFileLoc, interp2, xresA, yresA, zresA, xdimA, ydimA, zdimA, true, false, pad, message);
                Preferences.debug("Saved " + mtxFileLoc + "\n",Preferences.DEBUG_FILEIO);
                regAlgo3D.disposeLocal();
                regAlgo3D = null;
                baseImage.disposeLocal();
                baseImage = null;
                transformImage.disposeLocal();
                transformImage = null;
            } // if (regAlgo3D.isCompleted())
            else {
                MipavUtil.displayError("AlgorithmRegOAR3D did not complete successfully");
                setCompleted(false);
                return;
            }
        } // if (register)
    	
    	ThreadPoolExecutor exec = new ThreadPoolExecutor(concurrentNum, concurrentNum, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>());
        semaphore = new Semaphore(concurrentNum);
    	
        for(int i=0; i<transformImageAr.length; i++) {
            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            io.setTIFFOrientation(false);
            
            ModelImage baseImage = io.readImage(baseImageAr[i].getAbsolutePath());
            for (int j = 0; j < baseImage.getExtents()[2]; j++) {
                for (int k = 0; k < 3; k++) {
                    baseImage.getFileInfo(j).setUnitsOfMeasure(FileInfoBase.MICROMETERS, k);
                }
            }
            ModelImage transformImage = io.readImage(transformImageAr[i].getAbsolutePath());
            io = null;
            for (int j = 0; j < transformImage.getExtents()[2]; j++) {
                for (int k = 0; k < 3; k++) {
                    transformImage.getFileInfo(j).setUnitsOfMeasure(FileInfoBase.MICROMETERS, k);
                }
            }
            
            try {
                semaphore.acquire();
            }
            catch (InterruptedException e) {
                System.out.println("Interrupted exception on semaphore.acquire() " + e);    
            }
            FusionAlg algInstance = new FusionAlg(this, baseImage, transformImage);
            exec.execute(algInstance);

        }
        
        exec.shutdown();  
        
        try {
            exec.awaitTermination(1, TimeUnit.DAYS);
        } catch (InterruptedException e) {
            MipavUtil.displayError("Program did not execute correctly");
            e.printStackTrace();
        }
        
        ViewUserInterface.getReference().setAppFrameVisible(appFrameFlag);
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }
    
    
    public class MeasureAlg implements Runnable {

        private ModelImage baseImage, transformImage;

        private int xExtents, yExtents, zExtents;
        
        public MeasureAlg(ModelImage baseImage, ModelImage transformImage) {
            this.baseImage = baseImage;
            this.transformImage = transformImage;
        }
        
        public void run() {       
            test();   
        }

        private void test() {
            //fireProgressStateChanged(5, "Measure", "Measure init");
            
            xExtents = baseImage.getExtents()[0] < transformImage.getExtents()[0] ? baseImage.getExtents()[0] : transformImage.getExtents()[0];
            yExtents = baseImage.getExtents()[1] < transformImage.getExtents()[1] ? baseImage.getExtents()[1] : transformImage.getExtents()[1];
            zExtents = baseImage.getExtents()[2] < transformImage.getExtents()[2] ? baseImage.getExtents()[2] : transformImage.getExtents()[2];
            
            ArrayList<GenerateSum> sumList = new ArrayList<GenerateSum>();
            ArrayList<Future<Double>> futureList = new ArrayList<Future<Double>>();
            
            ExecutorService exec = Executors.newFixedThreadPool(concurrentNum);
            GenerateSum g = null;
            
            for(int i=minX; i<maxX; i+=stepSize) {
                g = new GenerateSum(i, minY, minZ, maxY, maxZ, stepSize);
                sumList.add(g);
                futureList.add(exec.submit(g));
            }
            
            exec.shutdown();
            
            double firstMinValue = Double.MAX_VALUE;
            
            int firstMinX = minX, firstMinY = minY, firstMinZ = minZ,
                secondMinX = minX, secondMinY = minY, secondMinZ = minZ,
                    thirdMinX = minX, thirdMinY = minY, thirdMinZ = minZ;
            
            try {
                for(int i=0; i<futureList.size(); i++) {
                    //fireProgressStateChanged((int) (100*(((double)i)/futureList.size())), "Measure", "Pass 1 fitting");
                    System.out.println(i);
                    if(futureList.get(i).get() < firstMinValue) {
                        thirdMinX = secondMinX;
                        thirdMinY = secondMinY;
                        thirdMinZ = secondMinZ;
                        
                        secondMinX = firstMinX;
                        secondMinY = firstMinY;
                        secondMinZ = firstMinZ;
                        
                        firstMinValue = futureList.get(i).get();
                        firstMinX = sumList.get(i).getXMeasure();
                        firstMinY = sumList.get(i).getYMeasure();
                        firstMinZ = sumList.get(i).getZMeasure();
                    }
                }
            } catch(Exception exe) {
                exe.printStackTrace();
            }
            
            //fireProgressStateChanged(10, "Measure", "Measure init");
            
            int minX = Math.min(Math.min(firstMinX, secondMinX), thirdMinX);
            int minY = Math.min(Math.min(firstMinY, secondMinY), thirdMinY);
            int minZ = Math.min(Math.min(firstMinZ, secondMinZ), thirdMinZ);
            
            minX -= stepSize;
            minY -= stepSize;
            minZ -= stepSize;
            
            int maxX = Math.max(Math.max(firstMinX, secondMinX), thirdMinX);
            int maxY = Math.max(Math.max(firstMinY, secondMinY), thirdMinY);
            int maxZ = Math.max(Math.max(firstMinZ, secondMinZ), thirdMinZ);
            
            maxX += stepSize;
            maxY += stepSize;
            maxZ += stepSize;
            
            Preferences.data("Range after first fitting attempt: \n");
            Preferences.data("("+minX+", "+minY+", "+minZ+") to ("+maxX+", "+maxY+", "+maxZ+")\n");
            
            sumList = new ArrayList<GenerateSum>();
            futureList = new ArrayList<Future<Double>>();
            
            exec = Executors.newFixedThreadPool(concurrentNum);
            g = null;
            
            for(int i=minX; i<maxX; i++) {
                g = new GenerateSum(i, minY, minZ, maxY, maxZ, 1);
                sumList.add(g);
                futureList.add(exec.submit(g));
            }
            
            exec.shutdown();
            
            try {
                for(int i=0; i<futureList.size(); i++) {
                    //fireProgressStateChanged((int) (100*(((double)i)/futureList.size())), "Measure", "Pass 2 fitting");
                    System.out.println(i);
                    if(futureList.get(i).get() < firstMinValue) {
                        minX = sumList.get(i).getXMeasure();
                        minY = sumList.get(i).getYMeasure();
                        minZ = sumList.get(i).getZMeasure();
                    }
                }
            } catch(Exception exe) {
                exe.printStackTrace();
            }
            
            Preferences.data("Mininimum match location found at: "+minX+", "+minY+", "+minZ+"\n");
            
            setMeasures(minX, minY, minZ);
            
        }

        private void setMeasures(int minX, int minY, int minZ) {
            xMovement = minX;
            yMovement = minY;
            zMovement = minZ;
        }
        
        public class GenerateSum implements Callable<Double> {

            private int xMeasure;
            private int yMeasure;
            private int zMeasure;
            private int stepSize;
            private int minY;
            private int minZ;
            private int maxY;
            private int maxZ;

            public GenerateSum(int xMeasure, int minY, int minZ, int maxY, int maxZ, int stepSize) {
                this.xMeasure = xMeasure;
                
                this.minY = minY;
                this.minZ = minZ;
                
                this.maxY = maxY;
                
                this.maxZ = maxZ;
                
                this.stepSize = stepSize;
            }
            
            public Double call() {
                double minSum = Double.MAX_VALUE;
                
                for(int yMeasure=minY; yMeasure<maxY; yMeasure+=stepSize) {
                    for(int zMeasure=minZ; zMeasure<maxZ; zMeasure+=stepSize) {
                        double sum = performSum(xMeasure, yMeasure, zMeasure);
                        if(sum < minSum) {
                            this.yMeasure = yMeasure;
                            this.zMeasure = zMeasure;
                            minSum = sum;
                        }
                    }
                }
                
                return minSum;
            }

            private double performSum(int xMeasure, int yMeasure, int zMeasure) {
                double sumAmount = 0;
                
                double baseIntensity = 0, transformIntensity = 0;
                
                double sumCutoff = (thresholdIntensity == 0) ? 20 : thresholdIntensity;
                
                int transformX, transformY, transformZ;
                //new ViewJFrameImage(transformImage);
                for(int i=0; i<xExtents; i++) {
                    transformX = i-xMeasure;
                    for(int j=0; j<yExtents; j++) {
                        transformY = j-yMeasure;
                        for(int k=0; k<zExtents; k++) {
                            transformZ = k-zMeasure;
                            baseIntensity = baseImage.getDouble(i, j, k);
                            
                            if(i < baseImage.getExtents()[0] && 
                                    j < baseImage.getExtents()[1] && 
                                    k < baseImage.getExtents()[2]) {
                                baseIntensity = baseImage.getDouble(i, j, k);
                            }
                            
                            if(transformX >= 0 && transformX < transformImage.getExtents()[0] && 
                                    transformY >= 0 && transformY < transformImage.getExtents()[1] && 
                                    transformZ >= 0 && transformZ < transformImage.getExtents()[2]) {
                                transformIntensity = transformImage.getDouble(transformX, transformY, transformZ);
                            }
                            
                            if(baseIntensity < sumCutoff) { 
                                baseIntensity = 0;
                            }
                            if(transformX >= 0 && transformX < transformImage.getExtents()[0] && 
                                    transformY >= 0 && transformY < transformImage.getExtents()[1] && 
                                    transformZ >= 0 && transformZ < transformImage.getExtents()[2]) {
                                transformIntensity = transformImage.getDouble(transformX, transformY, transformZ);
                                if(transformIntensity < 20) {
                                    transformIntensity = 0;
                                }
                                sumAmount += Math.abs((baseIntensity - transformIntensity));
                            } else {
                                sumAmount += Math.abs(baseIntensity - 0);
                            }
                        }
                    }
                }
                
                return sumAmount;
            }

            public int getXMeasure() {
                return xMeasure;
            }

            public int getYMeasure() {
                return yMeasure;
            }

            public int getZMeasure() {
                return zMeasure;
            }
            
        }
    }

    public class FusionAlg implements Runnable {

        private ModelImage baseImage, transformImage;
        
        private String baseImageName, transformImageName;
        
        private ModelImage subGeoImage = null, subAriImage = null;
        
        private float[] finalRes;
        
        private Object parentObject;
        
        public FusionAlg(Object parentObject, ModelImage baseImage, ModelImage transformImage) {
            this.parentObject = parentObject;
            this.baseImage = baseImage;
            this.baseImageName = baseImage.getImageName();
            this.transformImage = transformImage;
            this.transformImageName = transformImage.getImageName();
        }
        
        public void run() {
            call();
            semaphore.release();
        }
        
        public Boolean call() {
            //fireProgressStateChanged(5, "Transform", "Rotating");
            System.out.println("Processing " + baseImage.getImageName() + " and " + transformImage.getImageName());

            baseImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            transformImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            finalRes = new float[baseImage.getResolutions(0).length];
            
            for(int i=0; i<baseImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(baseImage.getResolutions(i)[2]);
            }
            
            for(int i=0; i<transformImage.getFileInfo().length; i++) {
                transformImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            
            if(doInterImages) {
                resultImageList.add(baseImage);
            }
            
            transformImage = rotate(transformImage, AlgorithmRotate.Y_AXIS_MINUS);
            
            //fireProgressStateChanged(10, "Transform", "Performing "+mode.toString());
            
            transform();
            
            //fireProgressStateChanged(15, "Generating movements", "Performing "+mode.toString());
            
            synchronized(parentObject) {
                if(xMovement == null && yMovement == null && zMovement == null) {
                    if(exec == null) {
                        exec = Executors.newFixedThreadPool(1);
                        System.out.println("Print once");
                        //fireProgressStateChanged(15, "Transform", "Launching measure algorithm");
                        MeasureAlg alg = new MeasureAlg(baseImage, transformImage);
                        exec.submit(alg);
                        exec.shutdown();
                    }
                    
                    try {
                        exec.awaitTermination(1, TimeUnit.DAYS);
                    } catch (InterruptedException e) {
                        MipavUtil.displayError("Time point "+transformImage.getImageName()+" will be incomplete.");
                        e.printStackTrace();
                    }
                }
            }
         
            //fireProgressStateChanged(20, "Transform", "Performing "+mode.toString());
            
            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            FileWriteOptions options = new FileWriteOptions(null, null, true);
            saveType = saveType.toLowerCase();
            if(saveType.contains("raw")) {
            	options.setFileType(FileUtility.XML);
            } else {
            	options.setFileType(FileUtility.TIFF);
            }
            
            options.setIsScript(true);
            options.setOptionsSet(true);
            
            FileInfoBase resultImageInfoBase = null;
            
            switch (mode) {
            
            case DownsampleToBase:
                //downsampleToBase(); transform image has already been downsampled
                resultImageInfoBase = baseImage.getFileInfo(0);
                for(int i=0; i<baseImage.getResolutions(0).length; i++) {
                    finalRes[i] = baseImage.getResolutions(0)[i];
                }
                break;
            
            case DownsampleUpsampleCombined:
                downsampleUpsampleCombined();
                
                resultImageInfoBase = baseImage.getFileInfo(0);
                int[] dim = new int[baseImage.getExtents().length];
                for(int i=0; i<baseImage.getExtents().length; i++) {
                    dim[i] = (int) ((((double)baseImage.getExtents()[i]) + transformImage.getExtents()[i]) / 2.0);
                }
                resultImageInfoBase.setExtents(dim);
                
                break;
                
            case UpsampleToTransform:
                upsampleToTransform();
                
                resultImageInfoBase = baseImage.getFileInfo(0);
                resultImageInfoBase.setExtents(new int[]{transformImage.getExtents()[0], baseImage.getExtents()[1], baseImage.getExtents()[2]});
                break;
            }
            
            resultImageInfoBase.setResolutions(finalRes);
            
            if(showGeoMean || saveGeoMean) {
                subGeoImage = ViewUserInterface.getReference().createBlankImage((FileInfoBase) resultImageInfoBase.clone(), false);
            }
            if(showAriMean || saveAriMean) {
                subAriImage = ViewUserInterface.getReference().createBlankImage((FileInfoBase) resultImageInfoBase.clone(), false);
            }
            
            if(doThreshold) {
                threshold(baseImage, thresholdIntensity);
                threshold(transformImage, thresholdIntensity); //all transformations are complete
            }
            
            if(doShowPrefusion || doSavePrefusion || doDeconv) {
                //fireProgressStateChanged(15, "Transform", "Creating prefusion images");
                
                ModelImage prefusionTransformImage = ViewUserInterface.getReference().createBlankImage((FileInfoBase) resultImageInfoBase.clone(), false);
                ModelImage prefusionBaseImage = ViewUserInterface.getReference().createBlankImage((FileInfoBase) resultImageInfoBase.clone(), false);
                //new ViewJFrameImage(transformImage);
                int transformX, transformY, transformZ;        
                
                for(int i=0; i<resultImageInfoBase.getExtents()[0]; i++) {
                    transformX = i-xMovement;
                    for(int j=0; j<resultImageInfoBase.getExtents()[1]; j++) {
                        transformY = j-yMovement;
                        for(int k=0; k<resultImageInfoBase.getExtents()[2]; k++) {
                            transformZ = k-zMovement;
                            if(transformX >= 0 && transformX < transformImage.getExtents()[0] && transformX < baseImage.getExtents()[0] && 
                                    transformY >= 0 && transformY < transformImage.getExtents()[1] && transformY < baseImage.getExtents()[1] && 
                                    transformZ >= 0 && transformZ < transformImage.getExtents()[2] && transformZ < baseImage.getExtents()[2]) {
                                prefusionTransformImage.set(i, j, k, transformImage.getDouble(transformX, transformY, transformZ));
                            }
                            if(i < baseImage.getExtents()[0] && 
                                j < baseImage.getExtents()[1] &&
                                k < baseImage.getExtents()[2]) {
                                prefusionBaseImage.set(i, j, k, baseImage.getDouble(i, j, k));
                            }
                        }
                    }
                }
                
                prefusionTransformImage.calcMinMax();
                prefusionTransformImage.setImageName("Prefusion_"+transformImageName);
                prefusionBaseImage.calcMinMax();
                prefusionBaseImage.setImageName("Prefusion_"+baseImageName);
                
                if(doSavePrefusion) {
                    options.setFileDirectory(prefusionBaseDir.getAbsolutePath()+File.separator);
                    options.setFileName(prefusionBaseImage.getImageFileName());
                    options.setBeginSlice(0);
                    options.setEndSlice(prefusionBaseImage.getExtents()[2]-1);
                    io.writeImage(prefusionBaseImage, options, false);
                    
                    options.setFileDirectory(prefusionTransformDir.getAbsolutePath()+File.separator);
                    options.setFileName(prefusionTransformImage.getImageFileName());
                    options.setBeginSlice(0);
                    options.setEndSlice(prefusionTransformImage.getExtents()[2]-1);
                    io.writeImage(prefusionTransformImage, options, false);
                }
                
                doMaxProj(prefusionBaseImage, doShowPrefusion, doSavePrefusion, prefusionBaseDir, options, io);
                doMaxProj(prefusionTransformImage, doShowPrefusion, doSavePrefusion, prefusionTransformDir, options, io);
                
                if(doShowPrefusion) {
                    resultImageList.add(prefusionTransformImage);
                    resultImageList.add(prefusionBaseImage);
                }
                
                if(doInterImages) {
                    new ViewJFrameImage(prefusionTransformImage);
                    new ViewJFrameImage(prefusionBaseImage);
                }
                
                if (doDeconv) {
                	ModelImage deconvImg = deconvolve(prefusionBaseImage, prefusionTransformImage);
                	
                	options.setFileDirectory(deconvDir.getAbsolutePath()+File.separator);
                    options.setFileName(deconvImg.getImageName());
                    options.setBeginSlice(0);
                    options.setEndSlice(deconvImg.getExtents()[2]-1);
                    io.writeImage(deconvImg, options, false);
                    
                    doMaxProj(deconvImg, false, true, deconvDir, options, io);
                    
                    deconvImg.disposeLocal(false);
                }
                
                if(!doShowPrefusion && !doInterImages) {
                    prefusionBaseImage.disposeLocal();
                    
                    prefusionTransformImage.disposeLocal();
                }
            }
            
            //fireProgressStateChanged(75, "Transform", "Calculating means");
            
            if(showAriMean || saveAriMean) {
                calcAriMean();
                if(saveMaxProj || showMaxProj) {
                    doMaxProj(subAriImage, showAriMean, saveAriMean, ariMeanDir, options, io);
                }
            }       
                
            if(saveAriMean) {
                options.setFileDirectory(ariMeanDir.getAbsolutePath()+File.separator);
                options.setFileName(subAriImage.getImageFileName());
                options.setBeginSlice(0);
                options.setEndSlice(subAriImage.getExtents()[2]-1);
                io.writeImage(subAriImage, options, false);
            }
            
            if(showAriMean) {
                resultImageList.add(subAriImage);
            } else if(saveAriMean && !doInterImages) {
                subAriImage.disposeLocal();
            }
            
            if(showGeoMean || saveGeoMean) {
                calcGeoMean();
                
                if(saveMaxProj || showMaxProj) {
                    doMaxProj(subGeoImage, showGeoMean, saveGeoMean, geoMeanDir, options, io);
                }
            }
            
            if(saveGeoMean) {
                options.setFileDirectory(geoMeanDir.getAbsolutePath()+File.separator);
                options.setFileName(subGeoImage.getImageFileName());
                options.setBeginSlice(0);
                options.setEndSlice(subGeoImage.getExtents()[2]-1);
                io.writeImage(subGeoImage, options, false);
            }
            
            if(showGeoMean) {
                resultImageList.add(subGeoImage);
            } else if(saveGeoMean && !doInterImages) {
                subGeoImage.disposeLocal();
            }
            
            if(!doInterImages) {
                baseImage.disposeLocal();
                
                transformImage.disposeLocal();
            }
            
            return true;
        }
        
        private void doMaxProj(ModelImage image, boolean parentShow, boolean parentSave, File parentDir, FileWriteOptions options, FileIO io) {
            if(showMaxProj || saveMaxProj) {
                AlgorithmMaximumIntensityProjection[] maxAlgoClone = new AlgorithmMaximumIntensityProjection[maxAlgo.length];
                for(int i=0; i<maxAlgoClone.length; i++) {
                    maxAlgoClone[i] = new AlgorithmMaximumIntensityProjection(maxAlgo[i].getSrcImage(), maxAlgo[i].getStartSlice(), maxAlgo[i].getStopSlice(), 
                                                maxAlgo[i].getWindow(), maxAlgo[i].getMinIntensity()[0], maxAlgo[i].getMaxIntensity()[0], maxAlgo[i].isComputeMaximum(), 
                                                maxAlgo[i].isComputeMinimum(), maxAlgo[i].getProjectionDirection());
                    maxAlgoClone[i].setQuiet(true);
                }
                
                for(int i=0; i<maxAlgoClone.length; i++) {
                    maxAlgoClone[i].setSrcImage(image);
                    maxAlgoClone[i].setStartSlice(0);
                    maxAlgoClone[i].setMaxIntensity(new double[]{image.getMax()});
                    switch(maxAlgoClone[i].getProjectionDirection()) {
                    case AlgorithmMaximumIntensityProjection.X_PROJECTION:
                        maxAlgoClone[i].setStopSlice(image.getExtents()[0]-1);
                        if(maxAlgoClone[i].getWindow() == -1 || maxAlgoClone[i].getWindow() > image.getExtents()[0]) {
                            maxAlgoClone[i].setWindow(image.getExtents()[0]);
                        }
                        break;
                        
                    case AlgorithmMaximumIntensityProjection.Y_PROJECTION:
                        maxAlgoClone[i].setStopSlice(image.getExtents()[1]-1);
                        if(maxAlgoClone[i].getWindow() == -1 || maxAlgoClone[i].getWindow() > image.getExtents()[1]) {
                            maxAlgoClone[i].setWindow(image.getExtents()[1]);
                        }
                        break;
                        
                    case AlgorithmMaximumIntensityProjection.Z_PROJECTION:
                        maxAlgoClone[i].setStopSlice(image.getExtents()[2]-1);
                        if(maxAlgoClone[i].getWindow() == -1 || maxAlgoClone[i].getWindow() > image.getExtents()[2]) {
                            maxAlgoClone[i].setWindow(image.getExtents()[2]);
                        }
                        break;
                    }
                    maxAlgoClone[i].setRunningInSeparateThread(false);
                    maxAlgoClone[i].run();
                }
                
                String projFolder = "Proj";
                for(int i=0; i<maxAlgoClone.length; i++) {
                    if(parentShow && showMaxProj) {
                        resultImageList.addAll(maxAlgoClone[i].getResultImage());
                    }
                    
                    switch(maxAlgoClone[i].getProjectionDirection()) {
                    case AlgorithmMaximumIntensityProjection.X_PROJECTION:
                        projFolder = PlugInDialogGenerateFusion.XPROJ;
                        break;
                        
                    case AlgorithmMaximumIntensityProjection.Y_PROJECTION:
                        projFolder = PlugInDialogGenerateFusion.YPROJ;
                        break;
                        
                    case AlgorithmMaximumIntensityProjection.Z_PROJECTION:
                        projFolder = PlugInDialogGenerateFusion.ZPROJ;
                        break;
                    }
                    
                    if(parentSave && saveMaxProj) {
                        Vector<ModelImage> resImageVec = maxAlgoClone[i].getResultImage();
                        for(int j=0; j<resImageVec.size(); j++) {
                            options.setFileDirectory(parentDir.getAbsolutePath()+File.separator+projFolder+File.separator);
                            options.setFileName(resImageVec.get(j).getImageFileName());
                            options.setBeginSlice(0);
                            options.setOptionsSet(true);
                            options.setSaveAs(false);
                            if(resImageVec.get(j).getNDims() > 2) {
                                options.setEndSlice(resImageVec.get(j).getExtents()[2]-1);
                            } else {
                                options.setEndSlice(0);
                            }
                            io.writeImage(resImageVec.get(j), options, false);
                        }
                        if (!(parentShow && showMaxProj)) {
                            for (int j = resImageVec.size() - 1; j >= 0; j--) {
                                ModelImage img = resImageVec.remove(j);
                                img.disposeLocal();
                            }
                            resImageVec = null;
                        }
                    }
                    maxAlgoClone[i].finalize();
                }
            }
        }

        

        private void transform() {
            int[] dim = new int[transformImage.getExtents().length];
            float[] res = new float[transformImage.getResolutions(0).length];
            float oXres = 1.0f;
            float oYres = 1.0f;
            float oZres = 1.0f;
            int oXdim = 0;
            int oYdim = 0;
            int oZdim = 0;
            
            switch (mode) {
            
            case DownsampleToBase:
                oXdim = baseImage.getExtents()[0];
                oYdim = baseImage.getExtents()[1];
                oZdim = baseImage.getExtents()[2];
                oXres = baseImage.getResolutions(0)[0];
                oYres = baseImage.getResolutions(0)[1];
                oZres = baseImage.getResolutions(0)[2];
                break;
            
            case UpsampleToTransform:
                for(int i=0; i<dim.length; i++) {
                    if(transformImage.getExtents()[i] > baseImage.getExtents()[i]) {
                        dim[i] = transformImage.getExtents()[i];
                    } else {
                        dim[i] = baseImage.getExtents()[i];
                    }
                }
                
                oXdim = dim[0];
                oYdim = dim[1];
                oZdim = dim[2];
                
                for(int i=0; i<res.length; i++) {
                    if(transformImage.getResolutions(0)[i] < baseImage.getResolutions(0)[i]) {
                        res[i] = transformImage.getResolutions(0)[i];
                    } else {
                        res[i] = baseImage.getResolutions(0)[i];
                    }
                }
                
                oXres = res[0];
                oYres = res[1];
                oZres = res[2];
                break;
            
            case DownsampleUpsampleCombined:
                
                for(int i=0; i<res.length; i++) {
                    if(transformImage.getResolutions(0)[i] == baseImage.getResolutions(0)[i]) {
                        res[i] = transformImage.getResolutions(0)[i];
                    } else if(transformImage.getResolutions(0)[i] > baseImage.getResolutions(0)[i]) {
                        res[i] = baseImage.getResolutions(0)[i] / transformImage.getResolutions(0)[i]; 
                    } else { //transform res < base res
                        res[i] = transformImage.getResolutions(0)[i] / baseImage.getResolutions(0)[i];
                    }
                }
                
                for(int i=0; i<dim.length; i++) {
                    dim[i] = (int) ((baseImage.getExtents()[i] + transformImage.getExtents()[i]) / 2.0); 
                }
                
                oXdim = dim[0];
                oYdim = dim[1];
                oZdim = dim[2];
                oXres = res[0];
                oYres = res[1];
                oZres = res[2];
                break;
                
            }

            boolean doPad = false;
            TransMatrix xfrm = new TransMatrix(4);
            xfrm.identity();
            if (mtxFileLoc == null) {
                MipavUtil.displayError("mtxFileLoc = null");
            }
            try {
                // search for file name relative to image first, then relative to MIPAV default, then absolute path
                File file = null;
                
                file = new File(transformImage.getImageDirectory() + mtxFileLoc);
                if ( !file.exists()) {
                    file = new File(ViewUserInterface.getReference().getDefaultDirectory() + mtxFileLoc);
                }
                if ( !file.exists()) {
                    file = new File(mtxFileLoc);
                }

                final RandomAccessFile raFile = new RandomAccessFile(file, "r");
                
                int fileInterp[] = new int[1];
                float fileXres[] = new float[1];
                float fileYres[] = new float[1];
                float fileZres[] = new float[1];
                int fileXdim[] = new int[1];
                int fileYdim[] = new int[1];
                int fileZdim[] = new int[1];
                boolean filetVOI[] = new boolean[1];
                boolean fileClip[] = new boolean[1];
                boolean filePad[] = new boolean[1];
                xfrm.readMatrix(raFile, fileInterp, fileXres, fileYres, fileZres, fileXdim, fileYdim, fileZdim, 
                                      filetVOI, fileClip, filePad, false);
                raFile.close();

                // We don't know the coordinate system that the transformation represents. Therefore
                // bring up a dialog where the user can ID the coordinate system changes (i.e.
                // world coordinate and/or the "left-hand" coordinate system!
                // new JDialogOrientMatrix(parentFrame, (JDialogBase) this);
            } catch (final IOException error) {
                MipavUtil.displayError("Matrix read error");
                xfrm.identity();
            }
            int interp = AlgorithmTransform.TRILINEAR;
            int units[] = new int[3];
            units[0] = baseImage.getUnitsOfMeasure()[0];
            units[1] = baseImage.getUnitsOfMeasure()[1];
            units[2] = baseImage.getUnitsOfMeasure()[2];
            boolean doClip = true;
            boolean doVOI = false;
            boolean doRotateCenter = false;
            Vector3f center = new Vector3f();
            float fillValue = 0.0f;
            boolean doUpdateOrigin = false;
            boolean isSATransform = false;
            AlgorithmTransform algoTrans = new AlgorithmTransform(transformImage, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
                    doVOI, doClip, doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.setUseScannerAnatomical(isSATransform);
            algoTrans.setSuppressProgressBar(true);
            
            algoTrans.run();
            
            if(!doInterImages) {
                transformImage.disposeLocal();
            }
            
            transformImage = algoTrans.getTransformedImage();
            algoTrans.disposeLocal();
            algoTrans = null;
            transformImage.calcMinMax();
            
            if(doInterImages) {
                new ViewJFrameImage(transformImage);
            }
        }

        
        private ModelImage subTransform(ModelImage image, TransMatrix mat, int[] outDim, float[] outRes) {
            boolean doPad = true;
            int interp = AlgorithmTransform.TRILINEAR;
            int units[] = new int[3];
            units[0] = image.getUnitsOfMeasure()[0];
            units[1] = image.getUnitsOfMeasure()[1];
            units[2] = image.getUnitsOfMeasure()[2];
            boolean doClip = true;
            boolean doVOI = false;
            boolean doRotateCenter = false;
            Vector3f center = new Vector3f();
            float fillValue = 0.0f;
            boolean doUpdateOrigin = false;
            boolean isSATransform = false;
            AlgorithmTransform algoTrans = new AlgorithmTransform(image, mat, interp, outRes[0], outRes[1], outRes[2], 
                    outDim[0], outDim[1], outDim[2], units,
                    doVOI, doClip, doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.setUseScannerAnatomical(isSATransform);
            algoTrans.setSuppressProgressBar(true);
            
            algoTrans.run();
            
            if(!doInterImages) {
                image.disposeLocal();
            }
            
            ModelImage subImage = algoTrans.getTransformedImage();
            algoTrans.disposeLocal();
            algoTrans = null;
            subImage.calcMinMax();
            return subImage;
        }

        private void downsampleUpsampleCombined() {
            
            TransMatrix mat = new TransMatrix(4);
            mat.identity();
            mat.set(0, 0, baseImage.getResolutions(0)[0] / transformImage.getResolutions(0)[0]);
            
            mat.set(2, 2, baseImage.getResolutions(0)[2] / transformImage.getResolutions(0)[2]);
            
            baseImage = subTransform(baseImage, mat, baseImage.getExtents(), baseImage.getResolutions(0));
            
            baseImage.setResolutions(new float[]{transformImage.getResolutions(0)[0], transformImage.getResolutions(0)[1], transformImage.getResolutions(0)[2]});
            for(int i=0; i<baseImage.getResolutions(0).length; i++) {
                finalRes[i] = baseImage.getResolutions(0)[i];
            }
            for(int i=0; i<baseImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            if(doInterImages) {
                new ViewJFrameImage(baseImage);
            }
        }
        
        private void upsampleToTransform() {
            TransMatrix mat = new TransMatrix(4);
            mat.identity();
            mat.set(0, 0, baseImage.getResolutions(0)[0] / transformImage.getResolutions(0)[0]);
            mat.set(2, 2, baseImage.getResolutions(0)[2] / transformImage.getResolutions(0)[2]);
            
            baseImage = subTransform(baseImage, mat, baseImage.getExtents(), baseImage.getResolutions(0));
            
            for(int i=0; i<baseImage.getResolutions(0).length; i++) {
                finalRes[i] = transformImage.getResolutions(0)[i];
            }
            for(int i=0; i<baseImage.getFileInfo().length && i<transformImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            if(doInterImages) {
                new ViewJFrameImage(baseImage);
            }
        }
        
        private ModelImage rotate(ModelImage image, int mode) {
            AlgorithmRotate rotate = new AlgorithmRotate(image, mode);
            rotate.setQuiet(true);
            rotate.run(); //transform image replaced
            image.disposeLocal();
            image = rotate.getDestImage();
            rotate.finalize();
            if(doInterImages) {
                new ViewJFrameImage(image);
            }
            
            return image;
            
        }

        private void calcGeoMean() {
            
            int transformX, transformY, transformZ;
            double baseVal = 0, transVal = 0;
            double mult = 0;
            //new ViewJFrameImage(transformImage);
            for(int i=0; i<subGeoImage.getExtents()[0]; i++) {
                transformX = i-xMovement;
                for(int j=0; j<subGeoImage.getExtents()[1]; j++) {
                    transformY = j-yMovement;
                    for(int k=0; k<subGeoImage.getExtents()[2]; k++) {
                        mult = 0;
                        baseVal = 0;
                        transVal = 0;
                        transformZ = k-zMovement;
                        if(transformX >= 0 && transformX < transformImage.getExtents()[0] && 
                                transformY >= 0 && transformY < transformImage.getExtents()[1] && 
                                transformZ >= 0 && transformZ < transformImage.getExtents()[2]) {
                            transVal = transformImage.getDouble(transformX, transformY, transformZ);
                            mult+=transformGeoWeight;
                        }
                        if(i < baseImage.getExtents()[0] && 
                                j < baseImage.getExtents()[1] && 
                                k < baseImage.getExtents()[2]) {
                            baseVal = baseImage.getDouble(i, j, k);
                            mult+=baseGeoWeight;
                        }
                        
                        subGeoImage.set(i, j, k, Math.exp((transformGeoWeight*Math.log(transVal) + baseGeoWeight*Math.log(baseVal)) / mult));
                    }
                }
            }
            
            Preferences.data("Produced weighted geometric mean: Transformed weight: "+transformGeoWeight+"\tBase weight: "+baseGeoWeight+"\n");
            
            subGeoImage.calcMinMax();
            
            subGeoImage.setImageName("GeoMeanFused_"+baseImageName);
            
            if(doInterImages) {
                new ViewJFrameImage(subGeoImage);
            }
        }

        private void calcAriMean() {
            
            int transformX, transformY, transformZ;
            double baseVal = 0, transVal = 0;
            double mult = 0;
            for(int i=0; i<subAriImage.getExtents()[0]; i++) {
                transformX = i-xMovement;
                for(int j=0; j<subAriImage.getExtents()[1]; j++) {
                    transformY = j-yMovement;
                    for(int k=0; k<subAriImage.getExtents()[2]; k++) {
                        mult = 0;
                        baseVal = 0;
                        transVal = 0;
                        transformZ = k-zMovement;
                        if(transformX >= 0 && transformX < transformImage.getExtents()[0] && 
                                transformY >= 0 && transformY < transformImage.getExtents()[1] && 
                                transformZ >= 0 && transformZ < transformImage.getExtents()[2]) {
                            transVal = transformImage.getDouble(transformX, transformY, transformZ);
                            mult+=transformAriWeight;
                        }
                        if(i < baseImage.getExtents()[0] && 
                                j < baseImage.getExtents()[1] && 
                                k < baseImage.getExtents()[2]) {
                            baseVal = baseImage.getDouble(i, j, k);
                            mult+=baseAriWeight;
                        }
                        if(mult == 0) {
                            subAriImage.set(i, j, k, 0);
                        } else {
                            subAriImage.set(i, j, k, (transVal*transformAriWeight+baseVal*baseAriWeight)/mult);
                        }
                    }
                }
            }
            
            Preferences.data("Produced weighted arithmetic mean: Transformed weight: "+transformAriWeight+"\tBase weight: "+baseAriWeight+"\n");
            
            subAriImage.calcMinMax();
            
            subAriImage.setImageName("AriMeanFused_"+baseImageName);
            
            if(doInterImages) {
                new ViewJFrameImage(subAriImage);
            }
        }
        
        /**
         * Performs deconvolution to combine two provided images.  Adds the deconvolved image to the result image list.
         * @param imageA The base image.
         * @param imageB The transform image.
         * @return The deconvolved image.
         */
        private ModelImage deconvolve(ModelImage imageA, ModelImage imageB) {
            int index = baseImageName.lastIndexOf("_");
            String name;
            if (index != -1) {
                name = "Decon" + baseImageName.substring(index);    
            }
            else {
                name = "Decon_" + baseImageName;
            }
        	ModelImage resultImage = (ModelImage) imageA.clone(name);
        	resultImage.setImageName(name);
        	
        	OpenCLAlgorithmDeconvolution deconvAlgo = new OpenCLAlgorithmDeconvolution(resultImage, imageA, imageB, deconvSigmaA, deconvSigmaB, true, deconvIterations, useDeconvSigmaConversionFactor);
        	deconvAlgo.setRed(true);
        	deconvAlgo.setGreen(true);
        	deconvAlgo.setBlue(true);
        	
        	// can only perform one deconvolution at a time because it uses the GPU
        	synchronized(parentObject) {
        		deconvAlgo.run();
        	}
        	
        	if (deconvShowResults) {
        		resultImageList.add(deconvAlgo.getDestImage());
        	}
        	
        	return deconvAlgo.getDestImage();
        }

        /**
         * Discards extraneous slices from transform image.
         */
        /*private void discardSlices(int middleSlice) {
            // TODO Auto-generated method stub
            
        }*/

        public ModelImage getSubGeoImage() {
            return subGeoImage;
        }

        public ModelImage getSubAriImage() {
            return subAriImage;
        }
    }

    private void threshold(ModelImage baseImage, double threshold) {
        for(int i=0; i<baseImage.getDataSize(); i++) {
            if(baseImage.getDouble(i) <= thresholdIntensity) {
                baseImage.set(i, 0);
            }
        }
    }
}