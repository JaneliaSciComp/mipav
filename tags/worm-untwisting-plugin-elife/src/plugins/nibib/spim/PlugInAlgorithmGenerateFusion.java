package nibib.spim;


// MIPAV is freely available from http://mipav.cit.nih.gov

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

/*****************************************************************
 ****************************************************************** 
 The MIPAV application is intended for research use only. This application has NOT been approved for ANY diagnostic
 * use by the Food and Drug Administration. There is currently no approval process pending.
 * 
 * This software may NOT be used for diagnostic purposes.
 * 
 ****************************************************************** 
 ******************************************************************/

import static org.jocl.CL.CL_MEM_COPY_HOST_PTR;
import static org.jocl.CL.clCreateBuffer;
import static org.jocl.CL.clEnqueueNDRangeKernel;
import static org.jocl.CL.clSetKernelArg;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions2D;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmDeconvolution;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR2D;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

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

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. By extending AlgorithmBase, it
 * has no more functionality than any other algorithm in MIPAV. No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInAlgorithmGenerateFusion extends AlgorithmBase {

    public enum SampleMode {
        UpsampleToTransform("Upsample base image to transformed"), DownsampleToBase(
                "Downsample transformed image to base"), DownsampleUpsampleCombined("Do both sampling mechanisms");

        private String str;

        SampleMode(final String str) {
            this.str = str;
        }

        @Override
        public String toString() {
            return str;
        }
    }

    private boolean registerOne = true;
    
    private boolean registerAll = false;
    
    private boolean noRegister2D = false;
    
    private boolean register2DOne = false;
    
    private boolean register2DAll = false;
    
    private File register2DFileDir = null;

    private boolean showAriMean = true;

    private boolean doShowPrefusion = false;

    private boolean doInterImages = false;

    private boolean showGeoMean = false;

    private final File[] baseImageAr;

    private final File[] transformImageAr;

    private final boolean doThreshold;

    private double resX = 1;

    private double resY = 1;

    private double resZ = 1;

    private final double thresholdIntensity;

    private String mtxFileLoc;

    private final String mtxFileDirectory;

    private final int timeIndex;

    private final int concurrentNum;

    private Integer xMovement;

    private Integer yMovement;

    private Integer zMovement;

    private final Collection<ModelImage> resultImageList;

    private final SampleMode mode;

    private final int stepSize;

    private final int minX, minY, minZ;

    private final int maxX, maxY, maxZ;

    private volatile ExecutorService exec = null;

    private final boolean saveAriMean, saveGeoMean;

    private final File ariMeanDir, geoMeanDir;

    private final boolean doSavePrefusion;

    private final File prefusionBaseDir, prefusionTransformDir;

    /** Weights to use for calculating arithmetic image means */
    private final double baseAriWeight, transformAriWeight;

    /** Weights to use for calculating geometric image means */
    private final double baseGeoWeight, transformGeoWeight;

    /** Whether maximum projections are shown and saved */
    private final boolean showMaxProj, saveMaxProj;

    /** Optional MIP algorithm */
    private final AlgorithmMaximumIntensityProjection[] maxAlgo;

    /** File format for saving result images */
    private String saveType;

    private final boolean doDeconv;
    
    private final static int JavaPlatform = 1;
    
    private final static int OpenCLPlatform = 2;
    
    private final int deconvPlatform;  

    private final boolean deconvShowResults;

    private final int deconvIterations;

    private final float[] deconvSigmaA, deconvSigmaB;

    private final boolean useDeconvSigmaConversionFactor;

    private final File deconvDir;

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
    
    private int baseRotation = -1;
    
    private int transformRotation = AlgorithmRotate.Y_AXIS_MINUS;
    
    /** Default case */
	public static final int JOINT_DECON = 1;
	
	/** Average deconvolution is based on the arithmetical mean of the two estimates from A and B separately */
	public static final int AVERAGE_DECON = 2;
	
	/** Multiplication deconvolution is based on the geometrical mean of the two estimates from A and B separately */
	public static final int MULTIPLICATION_DECON = 3;
    
    private int deconvolutionMethod = JOINT_DECON;
    
    private boolean allowScriptRecording = false;

    /**
     * Constructor.
     * 
     * @param registerOne
     * @param registerAll
     * @param noRegister2D
     * @param register2DOne
     * @param register2DAll
     * @param register2DFileDir
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
     * @param resultImage Result image model
     * @param srcImg Source image model.
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
     * @param deconvPlatform
     * @param deconvolutionMethod
     * @param deconvIterations
     * @param deconvSigmaA
     * @param deconvSigmaB
     * @param useDeconvSigmaConversionFactor
     * @param deconvDir
     * @param deconvShowResults
     * @param baseRotation
     * @param transformRotation
     */
    public PlugInAlgorithmGenerateFusion(final boolean registerOne, final boolean registerAll, 
    		final boolean noRegister2D, final boolean register2DOne, final boolean register2DAll, 
    		final File register2DFileDir, final float rotateBeginX, final float rotateEndX,
            final float coarseRateX, final float fineRateX, final float rotateBeginY, final float rotateEndY,
            final float coarseRateY, final float fineRateY, final float rotateBeginZ, final float rotateEndZ,
            final float coarseRateZ, final float fineRateZ, final boolean doShowPrefusion, final boolean doInterImages,
            final boolean doGeoMean, final boolean doAriMean, final boolean showMaxProj, final boolean doThreshold,
            final double resX, final double resY, final double resZ, final int concurrentNum,
            final double thresholdIntensity, final String mtxFileLoc, final String mtxFileDirectory,
            final int timeIndex, final File[] baseImageAr, final File[] transformImageAr, final Integer xMovement,
            final Integer yMovement, final Integer zMovement, final SampleMode mode, final int minX, final int minY,
            final int minZ, final int maxX, final int maxY, final int maxZ, final int stepSize,
            final boolean saveMaxProj, final boolean saveGeoMean, final File geoMeanDir, final boolean saveAriMean,
            final File ariMeanDir, final boolean doSavePrefusion, final File prefusionBaseDir,
            final File prefusionTransformDir, final double baseAriWeight, final double transformAriWeight,
            final double baseGeoWeight, final double transformGeoWeight,
            final AlgorithmMaximumIntensityProjection[] maxAlgo, final String saveType, final boolean doDeconv,
            final int deconvPlatform, final int deconvolutionMethod, final int deconvIterations, 
            final float[] deconvSigmaA, final float[] deconvSigmaB,
            final boolean useDeconvSigmaConversionFactor, final File deconvDir, final boolean deconvShowResults,
            final int baseRotation, final int transformRotation) {
        this.registerOne = registerOne;
        this.registerAll = registerAll;
        this.noRegister2D = noRegister2D;
        this.register2DOne = register2DOne;
        this.register2DAll = register2DAll;
        this.register2DFileDir = register2DFileDir;
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
        this.deconvPlatform = deconvPlatform;
        this.deconvolutionMethod = deconvolutionMethod;
        this.deconvIterations = deconvIterations;
        this.deconvSigmaA = deconvSigmaA;
        this.deconvSigmaB = deconvSigmaB;
        this.useDeconvSigmaConversionFactor = useDeconvSigmaConversionFactor;
        this.deconvDir = deconvDir;
        this.deconvShowResults = deconvShowResults;
        this.baseRotation = baseRotation;
        this.transformRotation = transformRotation;
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

        final boolean appFrameFlag = ViewUserInterface.getReference().isAppFrameVisible();
        ViewUserInterface.getReference().setAppFrameVisible(false);

        if (registerOne) {
            AlgorithmRegOAR3D regAlgo3D = null;
            final FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            io.setTIFFOrientation(false);
            ModelImage baseImage = io.readImage(baseImageAr[timeIndex].getAbsolutePath());
            for (int i = 0; i < baseImage.getExtents()[2]; i++) {
                for (int j = 0; j < 3; j++) {
                    baseImage.getFileInfo(i).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, j);
                }
            }
            final String baseImageName = baseImage.getImageName();
            ModelImage transformImage = io.readImage(transformImageAr[timeIndex].getAbsolutePath());
            for (int i = 0; i < transformImage.getExtents()[2]; i++) {
                for (int j = 0; j < 3; j++) {
                    transformImage.getFileInfo(i).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, j);
                }
            }
            final String transformImageName = transformImage.getImageName();
            baseImage.setResolutions(new float[] {(float) resX, (float) resY, (float) resZ});
            transformImage.setResolutions(new float[] {(float) resX, (float) resY, (float) resZ});

            for (int i = 0; i < baseImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(baseImage.getResolutions(i)[2]);
            }

            for (int i = 0; i < transformImage.getFileInfo().length; i++) {
                transformImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            
            if (baseRotation >= 0) {
	            final AlgorithmRotate rotate = new AlgorithmRotate(baseImage, baseRotation);
	            rotate.run(); // transform image replaced
	            ViewUserInterface.getReference().unRegisterImage(baseImage);
	            baseImage.disposeLocal();
	            baseImage = rotate.getDestImage();
	            rotate.finalize();
            }
            
            if (transformRotation >= 0) {
	            final AlgorithmRotate rotate = new AlgorithmRotate(transformImage, transformRotation);
	            rotate.run(); // transform image replaced
	            ViewUserInterface.getReference().unRegisterImage(transformImage);
	            transformImage.disposeLocal();
	            transformImage = rotate.getDestImage();
	            rotate.finalize();
            }
            
            final int cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
            final int DOF = 12;
            final int interp = AlgorithmTransform.TRILINEAR;
            final boolean maxResol = true;
            final boolean doSubsample = true;
            final boolean doMultiThread = Preferences.isMultiThreadingEnabled() && (ThreadUtil.getAvailableCores() > 1);
            final boolean fastMode = false;
            final int maxIterations = 2;
            final int numMinima = 3;
            regAlgo3D = new AlgorithmRegOAR3D(baseImage, transformImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                    coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ,
                    coarseRateZ, fineRateZ, maxResol, doSubsample, doMultiThread, fastMode, maxIterations, numMinima);
            regAlgo3D.run();
            if (regAlgo3D.isCompleted()) {
                final int xdimA = baseImage.getExtents()[0];
                final int ydimA = baseImage.getExtents()[1];
                final int zdimA = baseImage.getExtents()[2];
                final float xresA = baseImage.getFileInfo(0).getResolutions()[0];
                final float yresA = baseImage.getFileInfo(0).getResolutions()[1];
                final float zresA = baseImage.getFileInfo(0).getResolutions()[2];
                final TransMatrix finalMatrix = regAlgo3D.getTransform();
                // Do not do finalMatrix.Inverse()
                finalMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                final String costName = "CORRELATION_RATIO_SMOOTHED";
                String message = "Using cost function, " + costName;
                message += ", the cost is " + Double.toString(regAlgo3D.getAnswer()) + ".\n";
                message += "Some registration settings: \n";
                message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
                message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
                message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
                mtxFileLoc = mtxFileDirectory + File.separator + transformImageName + "_To_" + baseImageName + ".mtx";
                final int interp2 = AlgorithmTransform.TRILINEAR;
                final boolean pad = false;
                System.out.println("finalMatrix = " + finalMatrix);
                finalMatrix.saveMatrix(mtxFileLoc, interp2, xresA, yresA, zresA, xdimA, ydimA, zdimA, true, false, pad,
                        message);
                Preferences.debug("Saved " + mtxFileLoc + "\n", Preferences.DEBUG_FILEIO);
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
        } // if (registerOne)
        else if (register2DOne) {
        	AlgorithmRegOAR2D regAlgo2D = null;
            final FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            io.setTIFFOrientation(false);
            ModelImage baseImage = io.readImage(baseImageAr[timeIndex].getAbsolutePath());
            for (int j = 0; j < 2; j++) {
                baseImage.getFileInfo(0).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, j);
            }
            final String baseImageName = baseImage.getImageName();
            ModelImage transformImage = io.readImage(transformImageAr[timeIndex].getAbsolutePath());
            for (int j = 0; j < 2; j++) {
                transformImage.getFileInfo(0).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, j);
            }
            final String transformImageName = transformImage.getImageName();
            //baseImage.setResolutions(new float[] {(float) resX, (float) resY});
            //transformImage.setResolutions(new float[] {(float) resX, (float) resY});
        	final int cost = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
            final int DOF = 6;
            final int interp = AlgorithmTransform.BILINEAR;
            final boolean doSubsample = true;
            // Already in multithreads
            final boolean doMultiThread  = Preferences.isMultiThreadingEnabled() && (ThreadUtil.getAvailableCores() > 1);
            regAlgo2D = new AlgorithmRegOAR2D(baseImage, transformImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                    coarseRateX, fineRateX, doSubsample, doMultiThread);
            regAlgo2D.run();
            if (regAlgo2D.isCompleted()) {
                final int xdimA = baseImage.getExtents()[0];
                final int ydimA = baseImage.getExtents()[1];
                final float xresA = baseImage.getFileInfo(0).getResolutions()[0];
                final float yresA = baseImage.getFileInfo(0).getResolutions()[1];
                final TransMatrix finalMatrix  = regAlgo2D.getTransform();
                // Do not do finalMatrix.Inverse()
                finalMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                final String costName = "CORRELATION_RATIO_SMOOTHED";
                String message = "Using cost function, " + costName;
                message += ", the cost is " + Double.toString(regAlgo2D.getCost()) + ".\n";
                message += "Some registration settings: \n";
                message += "Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with a coarse rate of " + coarseRateX + " and fine rate of " + fineRateX + ".\n";
                mtxFileLoc = mtxFileDirectory + File.separator + transformImageName + "_To_" + baseImageName + ".mtx";
                final int interp2 = AlgorithmTransform.BILINEAR;
                final boolean pad = false;
                System.out.println("finalMatrix = " + finalMatrix);
                finalMatrix.saveMatrix(mtxFileLoc, interp2, xresA, yresA, 0.0f, xdimA, ydimA, 0, true, false, pad,
                        message);
                Preferences.debug("Saved " + mtxFileLoc + "\n", Preferences.DEBUG_FILEIO);
                regAlgo2D.disposeLocal();
                regAlgo2D = null;
                baseImage.disposeLocal();
                baseImage = null;
                transformImage.disposeLocal();
                transformImage = null;
            } // if (regAlgo2D.isCompleted())
            else {
                MipavUtil.displayError("AlgorithmRegOAR2D did not complete successfully");
                setCompleted(false);
                return;
            }
        } // else if (register2DOne)

        final ThreadPoolExecutor exec = new ThreadPoolExecutor(concurrentNum, concurrentNum, 0L, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable>());
        semaphore = new Semaphore(concurrentNum);

        for (int i = 0; i < transformImageAr.length; i++) {
            FileIO io = new FileIO();
            io.setQuiet(true);
            io.setSuppressProgressBar(true);
            io.setTIFFOrientation(false);

            final ModelImage baseImage = io.readImage(baseImageAr[i].getAbsolutePath());
            if (noRegister2D || register2DOne || register2DAll) {
                for (int k = 0; k < 2; k++) {
                	baseImage.getFileInfo(0).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, k);	
                }
            }
            else {
	            for (int j = 0; j < baseImage.getExtents()[2]; j++) {
	                for (int k = 0; k < 3; k++) {
	                    baseImage.getFileInfo(j).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, k);
	                }
	            }
            } // else 
            final ModelImage transformImage = io.readImage(transformImageAr[i].getAbsolutePath());
            io = null;
            if (noRegister2D || register2DOne || register2DAll) {
            	for (int k = 0; k < 2; k++) {
                	transformImage.getFileInfo(0).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, k);	
                }	
            }
            else {
	            for (int j = 0; j < transformImage.getExtents()[2]; j++) {
	                for (int k = 0; k < 3; k++) {
	                    transformImage.getFileInfo(j).setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, k);
	                }
	            }
            } // else

            try {
                semaphore.acquire();
            } catch (final InterruptedException e) {
                System.out.println("Interrupted exception on semaphore.acquire() " + e);
            }
            final FusionAlg algInstance = new FusionAlg(this, baseImage, transformImage);
            exec.execute(algInstance);

        }

        exec.shutdown();

        try {
            exec.awaitTermination(1, TimeUnit.DAYS);
        } catch (final InterruptedException e) {
            MipavUtil.displayError("Program did not execute correctly");
            e.printStackTrace();
        }

        ViewUserInterface.getReference().setAppFrameVisible(appFrameFlag);

        setCompleted(true); // indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }

    public class MeasureAlg implements Runnable {

        private final ModelImage baseImage, transformImage;

        private int xExtents, yExtents, zExtents;

        public MeasureAlg(final ModelImage baseImage, final ModelImage transformImage) {
            this.baseImage = baseImage;
            this.transformImage = transformImage;
        }

        @Override
        public void run() {
            test();
        }

        private void test() {
            // fireProgressStateChanged(5, "Measure", "Measure init");

            xExtents = baseImage.getExtents()[0] < transformImage.getExtents()[0] ? baseImage.getExtents()[0]
                    : transformImage.getExtents()[0];
            yExtents = baseImage.getExtents()[1] < transformImage.getExtents()[1] ? baseImage.getExtents()[1]
                    : transformImage.getExtents()[1];
            zExtents = baseImage.getExtents()[2] < transformImage.getExtents()[2] ? baseImage.getExtents()[2]
                    : transformImage.getExtents()[2];

            ArrayList<GenerateSum> sumList = new ArrayList<GenerateSum>();
            ArrayList<Future<Double>> futureList = new ArrayList<Future<Double>>();

            ExecutorService exec = Executors.newFixedThreadPool(concurrentNum);
            GenerateSum g = null;

            for (int i = minX; i < maxX; i += stepSize) {
                g = new GenerateSum(i, minY, minZ, maxY, maxZ, stepSize);
                sumList.add(g);
                futureList.add(exec.submit(g));
            }

            exec.shutdown();

            double firstMinValue = Double.MAX_VALUE;

            int firstMinX = minX, firstMinY = minY, firstMinZ = minZ, secondMinX = minX, secondMinY = minY, secondMinZ = minZ, thirdMinX = minX, thirdMinY = minY, thirdMinZ = minZ;

            try {
                for (int i = 0; i < futureList.size(); i++) {
                    // fireProgressStateChanged((int) (100*(((double)i)/futureList.size())), "Measure",
                    // "Pass 1 fitting");
                    System.out.println(i);
                    if (futureList.get(i).get() < firstMinValue) {
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
            } catch (final Exception exe) {
                exe.printStackTrace();
            }

            // fireProgressStateChanged(10, "Measure", "Measure init");

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
            Preferences.data("(" + minX + ", " + minY + ", " + minZ + ") to (" + maxX + ", " + maxY + ", " + maxZ
                    + ")\n");

            sumList = new ArrayList<GenerateSum>();
            futureList = new ArrayList<Future<Double>>();

            exec = Executors.newFixedThreadPool(concurrentNum);
            g = null;

            for (int i = minX; i < maxX; i++) {
                g = new GenerateSum(i, minY, minZ, maxY, maxZ, 1);
                sumList.add(g);
                futureList.add(exec.submit(g));
            }

            exec.shutdown();

            try {
                for (int i = 0; i < futureList.size(); i++) {
                    // fireProgressStateChanged((int) (100*(((double)i)/futureList.size())), "Measure",
                    // "Pass 2 fitting");
                    System.out.println(i);
                    if (futureList.get(i).get() < firstMinValue) {
                        minX = sumList.get(i).getXMeasure();
                        minY = sumList.get(i).getYMeasure();
                        minZ = sumList.get(i).getZMeasure();
                    }
                }
            } catch (final Exception exe) {
                exe.printStackTrace();
            }

            Preferences.data("Mininimum match location found at: " + minX + ", " + minY + ", " + minZ + "\n");

            setMeasures(minX, minY, minZ);

        }

        private void setMeasures(final int minX, final int minY, final int minZ) {
            xMovement = minX;
            yMovement = minY;
            zMovement = minZ;
        }

        public class GenerateSum implements Callable<Double> {

            private final int xMeasure;

            private int yMeasure;

            private int zMeasure;

            private final int stepSize;

            private final int minY;

            private final int minZ;

            private final int maxY;

            private final int maxZ;

            public GenerateSum(final int xMeasure, final int minY, final int minZ, final int maxY, final int maxZ,
                    final int stepSize) {
                this.xMeasure = xMeasure;

                this.minY = minY;
                this.minZ = minZ;

                this.maxY = maxY;

                this.maxZ = maxZ;

                this.stepSize = stepSize;
            }

            @Override
            public Double call() {
                double minSum = Double.MAX_VALUE;

                for (int yMeasure = minY; yMeasure < maxY; yMeasure += stepSize) {
                    for (int zMeasure = minZ; zMeasure < maxZ; zMeasure += stepSize) {
                        final double sum = performSum(xMeasure, yMeasure, zMeasure);
                        if (sum < minSum) {
                            this.yMeasure = yMeasure;
                            this.zMeasure = zMeasure;
                            minSum = sum;
                        }
                    }
                }

                return minSum;
            }

            private double performSum(final int xMeasure, final int yMeasure, final int zMeasure) {
                double sumAmount = 0;

                double baseIntensity = 0, transformIntensity = 0;

                final double sumCutoff = (thresholdIntensity == 0) ? 20 : thresholdIntensity;

                int transformX, transformY, transformZ;
                // new ViewJFrameImage(transformImage);
                for (int i = 0; i < xExtents; i++) {
                    transformX = i - xMeasure;
                    for (int j = 0; j < yExtents; j++) {
                        transformY = j - yMeasure;
                        for (int k = 0; k < zExtents; k++) {
                            transformZ = k - zMeasure;
                            baseIntensity = baseImage.getDouble(i, j, k);

                            if (i < baseImage.getExtents()[0] && j < baseImage.getExtents()[1]
                                    && k < baseImage.getExtents()[2]) {
                                baseIntensity = baseImage.getDouble(i, j, k);
                            }

                            if (transformX >= 0 && transformX < transformImage.getExtents()[0] && transformY >= 0
                                    && transformY < transformImage.getExtents()[1] && transformZ >= 0
                                    && transformZ < transformImage.getExtents()[2]) {
                                transformIntensity = transformImage.getDouble(transformX, transformY, transformZ);
                            }

                            if (baseIntensity < sumCutoff) {
                                baseIntensity = 0;
                            }
                            if (transformX >= 0 && transformX < transformImage.getExtents()[0] && transformY >= 0
                                    && transformY < transformImage.getExtents()[1] && transformZ >= 0
                                    && transformZ < transformImage.getExtents()[2]) {
                                transformIntensity = transformImage.getDouble(transformX, transformY, transformZ);
                                if (transformIntensity < 20) {
                                    transformIntensity = 0;
                                }
                                sumAmount += Math.abs( (baseIntensity - transformIntensity));
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

        private final String baseImageName, transformImageName;

        private ModelImage subGeoImage = null, subAriImage = null;

        private float[] finalRes;

        private final Object parentObject;

        public FusionAlg(final Object parentObject, final ModelImage baseImage, final ModelImage transformImage) {
            this.parentObject = parentObject;
            this.baseImage = baseImage;
            this.baseImageName = baseImage.getImageName();
            this.transformImage = transformImage;
            this.transformImageName = transformImage.getImageName();
        }

        @Override
        public void run() {
            call();
            semaphore.release();
        }

        public Boolean call() {
            // fireProgressStateChanged(5, "Transform", "Rotating");
            System.out.println("Processing " + baseImage.getImageName() + " and " + transformImage.getImageName());
            
            if (noRegister2D || register2DOne || register2DAll) {
            	//baseImage.setResolutions(new float[] {(float) resX, (float) resY});
 	            //transformImage.setResolutions(new float[] {(float) resX, (float) resY});
            	final int[] dim = new int[transformImage.getExtents().length];
                final float[] res = new float[transformImage.getResolutions(0).length];
                float oXres = 1.0f;
                float oYres = 1.0f;
                int oXdim = 0;
                int oYdim = 0;

                switch (mode) {

                    case DownsampleToBase:
                        oXdim = baseImage.getExtents()[0];
                        oYdim = baseImage.getExtents()[1];
                        oXres = baseImage.getResolutions(0)[0];
                        oYres = baseImage.getResolutions(0)[1];
                        break;

                    case UpsampleToTransform:
                        for (int i = 0; i < dim.length; i++) {
                            if (transformImage.getExtents()[i] > baseImage.getExtents()[i]) {
                                dim[i] = transformImage.getExtents()[i];
                            } else {
                                dim[i] = baseImage.getExtents()[i];
                            }
                        }

                        oXdim = dim[0];
                        oYdim = dim[1];

                        for (int i = 0; i < res.length; i++) {
                            if (transformImage.getResolutions(0)[i] < baseImage.getResolutions(0)[i]) {
                                res[i] = transformImage.getResolutions(0)[i];
                            } else {
                                res[i] = baseImage.getResolutions(0)[i];
                            }
                        }

                        oXres = res[0];
                        oYres = res[1];
                        break;

                    case DownsampleUpsampleCombined:

                        for (int i = 0; i < res.length; i++) {
                            if (transformImage.getResolutions(0)[i] == baseImage.getResolutions(0)[i]) {
                                res[i] = transformImage.getResolutions(0)[i];
                            } else if (transformImage.getResolutions(0)[i] > baseImage.getResolutions(0)[i]) {
                                res[i] = baseImage.getResolutions(0)[i] / transformImage.getResolutions(0)[i];
                            } else { // transform res < base res
                                res[i] = transformImage.getResolutions(0)[i] / baseImage.getResolutions(0)[i];
                            }
                        }

                        for (int i = 0; i < dim.length; i++) {
                            dim[i] = (int) ( (baseImage.getExtents()[i] + transformImage.getExtents()[i]) / 2.0);
                        }

                        oXdim = dim[0];
                        oYdim = dim[1];
                        oXres = res[0];
                        oYres = res[1];
                        break;

                }
            	final boolean doPad = false;
                TransMatrix xfrm = new TransMatrix(3);
                final int interp = AlgorithmTransform.BILINEAR;
                if (register2DAll) {
	            	final int cost = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
	                final int DOF = 6;
	                final boolean doSubsample = true;
	                // Already in multithreads
	                final boolean doMultiThread = false;
	                AlgorithmRegOAR2D regAlgo2D = new AlgorithmRegOAR2D(baseImage, transformImage, cost, DOF, interp, rotateBeginX, rotateEndX,
	                        coarseRateX, fineRateX, doSubsample, doMultiThread);
	                regAlgo2D.run();
	                if (regAlgo2D.isCompleted()) {
	                    final int xdimA = baseImage.getExtents()[0];
	                    final int ydimA = baseImage.getExtents()[1];
	                    final float xresA = baseImage.getFileInfo(0).getResolutions()[0];
	                    final float yresA = baseImage.getFileInfo(0).getResolutions()[1];
	                    xfrm = regAlgo2D.getTransform();
	                    // Do not do finalMatrix.Inverse()
	                    xfrm.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
	                    final String costName = "CORRELATION_RATIO_SMOOTHED";
	                    String message = "Using cost function, " + costName;
	                    message += ", the cost is " + Double.toString(regAlgo2D.getCost()) + ".\n";
	                    message += "Some registration settings: \n";
	                    message += "Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
	                    message += "with a coarse rate of " + coarseRateX + " and fine rate of " + fineRateX + ".\n";
	                    mtxFileLoc = mtxFileDirectory + File.separator + transformImageName + "_To_" + baseImageName + ".mtx";
	                    final int interp2 = AlgorithmTransform.BILINEAR;
	                    final boolean pad = false;
	                    System.out.println("xfrm = " + xfrm);
	                    xfrm.saveMatrix(mtxFileLoc, interp2, xresA, yresA, 0.0f, xdimA, ydimA, 0, true, false, pad,
	                            message);
	                    Preferences.debug("Saved " + mtxFileLoc + "\n", Preferences.DEBUG_FILEIO);
	                    regAlgo2D.disposeLocal();
	                    regAlgo2D = null;
	                } // if (regAlgo2D.isCompleted())
	                else {
	                    MipavUtil.displayError("AlgorithmRegOAR2D did not complete successfully");
	                    setCompleted(false);
	                    return false;
	                }
                } // if (register2DAll)
                else if (noRegister2D || register2DOne) {
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
    	
    	                final int fileInterp[] = new int[1];
    	                final float fileXres[] = new float[1];
    	                final float fileYres[] = new float[1];
    	                final float fileZres[] = new float[1];
    	                final int fileXdim[] = new int[1];
    	                final int fileYdim[] = new int[1];
    	                final int fileZdim[] = new int[1];
    	                final boolean filetVOI[] = new boolean[1];
    	                final boolean fileClip[] = new boolean[1];
    	                final boolean filePad[] = new boolean[1];
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
                } // else if (noRegister2D || register2DOne)
                
                final int units[] = new int[2];
                units[0] = FileInfoBase.UNKNOWN_MEASURE;
                units[1] = FileInfoBase.UNKNOWN_MEASURE;
                final boolean doClip = true;
                final boolean doVOI = false;
                final boolean doRotateCenter = false;
                final Vector3f center = new Vector3f();
                final float fillValue = 0.0f;
                final boolean doUpdateOrigin = false;
                final boolean isSATransform = false;
                AlgorithmTransform algoTrans = new AlgorithmTransform(transformImage, xfrm, interp, oXres, oYres,
                        oXdim, oYdim, units, doVOI, doClip, doPad, doRotateCenter, center);
                algoTrans.setFillValue(fillValue);
                algoTrans.setUpdateOriginFlag(doUpdateOrigin);
                algoTrans.setUseScannerAnatomical(isSATransform);
                algoTrans.setSuppressProgressBar(true);

                algoTrans.run();

                if ( !doInterImages) {
                    transformImage.disposeLocal();
                }

                transformImage = algoTrans.getTransformedImage();
                algoTrans.disposeLocal();
                algoTrans = null;
                transformImage.calcMinMax();

                if (doInterImages) {
                    new ViewJFrameImage(transformImage);
                }
                
                final FileIO io = new FileIO();
	            io.setQuiet(true);
	            io.setSuppressProgressBar(true);
	            final FileWriteOptions options = new FileWriteOptions(null, null, true);
	            saveType = saveType.toLowerCase();
	            if (saveType.contains("raw")) {
	                options.setFileType(FileUtility.XML);
	            } else {
	                options.setFileType(FileUtility.TIFF);
	            }
	
	            options.setIsScript(true);
	            options.setOptionsSet(true);
	            
	            options.setFileDirectory(register2DFileDir.getAbsolutePath() + File.separator);
                options.setFileName(transformImage.getImageFileName());
                io.writeImage(transformImage, options, false, allowScriptRecording);
            } // if (noRegister2D || register2DOne || register2DAll)
            else {
	            baseImage.setResolutions(new float[] {(float) resX, (float) resY, (float) resZ});
	            transformImage.setResolutions(new float[] {(float) resX, (float) resY, (float) resZ});
	            finalRes = new float[baseImage.getResolutions(0).length];
	
	            for (int i = 0; i < baseImage.getFileInfo().length; i++) {
	                baseImage.getFileInfo(i).setSliceThickness(baseImage.getResolutions(i)[2]);
	            }
	
	            for (int i = 0; i < transformImage.getFileInfo().length; i++) {
	                transformImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
	            }
	            
	            if (baseRotation >= 0) {
	                baseImage = rotate(baseImage, baseRotation);	
	            }
	            
	            if (doInterImages) {
	                resultImageList.add(baseImage);
	            }
	
	            if (transformRotation >= 0) {
	                transformImage = rotate(transformImage, transformRotation);
	            }
	
	            // fireProgressStateChanged(10, "Transform", "Performing "+mode.toString());
	
	            transform();
	
	            // fireProgressStateChanged(15, "Generating movements", "Performing "+mode.toString());
	
	            synchronized (parentObject) {
	                if (xMovement == null && yMovement == null && zMovement == null) {
	                    if (exec == null) {
	                        exec = Executors.newFixedThreadPool(1);
	                        System.out.println("Print once");
	                        // fireProgressStateChanged(15, "Transform", "Launching measure algorithm");
	                        final MeasureAlg alg = new MeasureAlg(baseImage, transformImage);
	                        exec.submit(alg);
	                        exec.shutdown();
	                    }
	
	                    try {
	                        exec.awaitTermination(1, TimeUnit.DAYS);
	                    } catch (final InterruptedException e) {
	                        MipavUtil.displayError("Time point " + transformImage.getImageName() + " will be incomplete.");
	                        e.printStackTrace();
	                    }
	                }
	            }
	
	            // fireProgressStateChanged(20, "Transform", "Performing "+mode.toString());
	
	            final FileIO io = new FileIO();
	            io.setQuiet(true);
	            io.setSuppressProgressBar(true);
	            final FileWriteOptions options = new FileWriteOptions(null, null, true);
	            saveType = saveType.toLowerCase();
	            if (saveType.contains("raw")) {
	                options.setFileType(FileUtility.XML);
	            } else {
	                options.setFileType(FileUtility.TIFF);
	            }
	
	            options.setIsScript(true);
	            options.setOptionsSet(true);
	
	            FileInfoBase resultImageInfoBase = null;
	
	            switch (mode) {
	
	                case DownsampleToBase:
	                    // downsampleToBase(); transform image has already been downsampled
	                    resultImageInfoBase = baseImage.getFileInfo(0);
	                    for (int i = 0; i < baseImage.getResolutions(0).length; i++) {
	                        finalRes[i] = baseImage.getResolutions(0)[i];
	                    }
	                    break;
	
	                case DownsampleUpsampleCombined:
	                    downsampleUpsampleCombined();
	
	                    resultImageInfoBase = baseImage.getFileInfo(0);
	                    final int[] dim = new int[baseImage.getExtents().length];
	                    for (int i = 0; i < baseImage.getExtents().length; i++) {
	                        dim[i] = (int) ( ( ((double) baseImage.getExtents()[i]) + transformImage.getExtents()[i]) / 2.0);
	                    }
	                    resultImageInfoBase.setExtents(dim);
	
	                    break;
	
	                case UpsampleToTransform:
	                    upsampleToTransform();
	
	                    resultImageInfoBase = baseImage.getFileInfo(0);
	                    resultImageInfoBase.setExtents(new int[] {transformImage.getExtents()[0],
	                            baseImage.getExtents()[1], baseImage.getExtents()[2]});
	                    break;
	            }
	
	            resultImageInfoBase.setResolutions(finalRes);
	            for (int j = 0; j < 3; j ++) {
	            	resultImageInfoBase.setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, j);
	            }
	
	            if (showGeoMean || saveGeoMean) {
	                subGeoImage = ViewUserInterface.getReference().createBlankImage(
	                        (FileInfoBase) resultImageInfoBase.clone(), false, allowScriptRecording);
	            }
	            if (showAriMean || saveAriMean) {
	                subAriImage = ViewUserInterface.getReference().createBlankImage(
	                        (FileInfoBase) resultImageInfoBase.clone(), false, allowScriptRecording);
	            }
	
	            if (doThreshold) {
	                threshold(baseImage, thresholdIntensity);
	                threshold(transformImage, thresholdIntensity); // all transformations are complete
	            }
	
	            if (doShowPrefusion || doSavePrefusion || doDeconv) {
	                // fireProgressStateChanged(15, "Transform", "Creating prefusion images");
	
	                final ModelImage prefusionTransformImage = ViewUserInterface.getReference().createBlankImage(
	                        (FileInfoBase) resultImageInfoBase.clone(), false, allowScriptRecording);
	                final ModelImage prefusionBaseImage = ViewUserInterface.getReference().createBlankImage(
	                        (FileInfoBase) resultImageInfoBase.clone(), false, allowScriptRecording);
	                // new ViewJFrameImage(transformImage);
	                int transformX, transformY, transformZ;
	
	                for (int i = 0; i < resultImageInfoBase.getExtents()[0]; i++) {
	                    transformX = i - xMovement;
	                    for (int j = 0; j < resultImageInfoBase.getExtents()[1]; j++) {
	                        transformY = j - yMovement;
	                        for (int k = 0; k < resultImageInfoBase.getExtents()[2]; k++) {
	                            transformZ = k - zMovement;
	                            if (transformX >= 0 && transformX < transformImage.getExtents()[0]
	                                    && transformX < baseImage.getExtents()[0] && transformY >= 0
	                                    && transformY < transformImage.getExtents()[1]
	                                    && transformY < baseImage.getExtents()[1] && transformZ >= 0
	                                    && transformZ < transformImage.getExtents()[2]
	                                    && transformZ < baseImage.getExtents()[2]) {
	                                prefusionTransformImage.set(i, j, k,
	                                        transformImage.getDouble(transformX, transformY, transformZ));
	                            }
	                            if (i < baseImage.getExtents()[0] && j < baseImage.getExtents()[1]
	                                    && k < baseImage.getExtents()[2]) {
	                                prefusionBaseImage.set(i, j, k, baseImage.getDouble(i, j, k));
	                            }
	                        }
	                    }
	                }
	
	                prefusionTransformImage.calcMinMax();
	                prefusionTransformImage.setImageName("Prefusion_" + transformImageName);
	                prefusionBaseImage.calcMinMax();
	                prefusionBaseImage.setImageName("Prefusion_" + baseImageName);
	
	                if (doSavePrefusion) {
	                    options.setFileDirectory(prefusionBaseDir.getAbsolutePath() + File.separator);
	                    options.setFileName(prefusionBaseImage.getImageFileName());
	                    options.setBeginSlice(0);
	                    options.setEndSlice(prefusionBaseImage.getExtents()[2] - 1);
	                    io.writeImage(prefusionBaseImage, options, false, allowScriptRecording);
	
	                    options.setFileDirectory(prefusionTransformDir.getAbsolutePath() + File.separator);
	                    options.setFileName(prefusionTransformImage.getImageFileName());
	                    options.setBeginSlice(0);
	                    options.setEndSlice(prefusionTransformImage.getExtents()[2] - 1);
	                    io.writeImage(prefusionTransformImage, options, false, allowScriptRecording);
	                }
	
	                doMaxProj(prefusionBaseImage, doShowPrefusion, doSavePrefusion, prefusionBaseDir, options, io);
	                doMaxProj(prefusionTransformImage, doShowPrefusion, doSavePrefusion, prefusionTransformDir, options, io);
	
	                if (doShowPrefusion) {
	                    resultImageList.add(prefusionTransformImage);
	                    resultImageList.add(prefusionBaseImage);
	                }
	
	                if (doInterImages) {
	                    new ViewJFrameImage(prefusionTransformImage);
	                    new ViewJFrameImage(prefusionBaseImage);
	                }
	
	                if (doDeconv) {
	                    final ModelImage deconvImg = deconvolve(prefusionBaseImage, prefusionTransformImage);
	
	                    options.setFileDirectory(deconvDir.getAbsolutePath() + File.separator);
	                    options.setFileName(deconvImg.getImageName());
	                    options.setBeginSlice(0);
	                    options.setEndSlice(deconvImg.getExtents()[2] - 1);
	                    io.writeImage(deconvImg, options, false, allowScriptRecording);
	
	                    doMaxProj(deconvImg, false, true, deconvDir, options, io);
	
	                    if (!deconvShowResults) {
	                        deconvImg.disposeLocal(false);
	                    }
	                }
	
	                if ( !doShowPrefusion && !doInterImages) {
	                    prefusionBaseImage.disposeLocal();
	
	                    prefusionTransformImage.disposeLocal();
	                }
	            }
	
	            // fireProgressStateChanged(75, "Transform", "Calculating means");
	
	            if (showAriMean || saveAriMean) {
	                calcAriMean();
	                if (saveMaxProj || showMaxProj) {
	                    doMaxProj(subAriImage, showAriMean, saveAriMean, ariMeanDir, options, io);
	                }
	            }
	
	            if (saveAriMean) {
	                options.setFileDirectory(ariMeanDir.getAbsolutePath() + File.separator);
	                options.setFileName(subAriImage.getImageFileName());
	                options.setBeginSlice(0);
	                options.setEndSlice(subAriImage.getExtents()[2] - 1);
	                io.writeImage(subAriImage, options, false, allowScriptRecording);
	            }
	
	            if (showAriMean) {
	                resultImageList.add(subAriImage);
	            } else if (saveAriMean && !doInterImages) {
	                subAriImage.disposeLocal();
	            }
	
	            if (showGeoMean || saveGeoMean) {
	                calcGeoMean();
	
	                if (saveMaxProj || showMaxProj) {
	                    doMaxProj(subGeoImage, showGeoMean, saveGeoMean, geoMeanDir, options, io);
	                }
	            }
	
	            if (saveGeoMean) {
	                options.setFileDirectory(geoMeanDir.getAbsolutePath() + File.separator);
	                options.setFileName(subGeoImage.getImageFileName());
	                options.setBeginSlice(0);
	                options.setEndSlice(subGeoImage.getExtents()[2] - 1);
	                io.writeImage(subGeoImage, options, false, allowScriptRecording);
	            }
	
	            if (showGeoMean) {
	                resultImageList.add(subGeoImage);
	            } else if (saveGeoMean && !doInterImages) {
	                subGeoImage.disposeLocal();
	            }
            } // else !register2D

            if ( !doInterImages) {
                baseImage.disposeLocal();

                transformImage.disposeLocal();
            }

            return true;
        }

        private void doMaxProj(final ModelImage image, final boolean parentShow, final boolean parentSave,
                final File parentDir, final FileWriteOptions options, final FileIO io) {
            if (showMaxProj || saveMaxProj) {
                final AlgorithmMaximumIntensityProjection[] maxAlgoClone = new AlgorithmMaximumIntensityProjection[maxAlgo.length];
                for (int i = 0; i < maxAlgoClone.length; i++) {
                    maxAlgoClone[i] = new AlgorithmMaximumIntensityProjection(maxAlgo[i].getSrcImage(),
                            maxAlgo[i].getStartSlice(), maxAlgo[i].getStopSlice(), maxAlgo[i].getWindow(),
                            maxAlgo[i].getMinIntensity()[0], maxAlgo[i].getMaxIntensity()[0],
                            maxAlgo[i].isComputeMaximum(), maxAlgo[i].isComputeMinimum(),
                            maxAlgo[i].getProjectionDirection());
                    maxAlgoClone[i].setQuiet(true);
                }

                for (int i = 0; i < maxAlgoClone.length; i++) {
                    maxAlgoClone[i].setSrcImage(image);
                    maxAlgoClone[i].setStartSlice(0);
                    maxAlgoClone[i].setMaxIntensity(new double[] {image.getMax()});
                    switch (maxAlgoClone[i].getProjectionDirection()) {
                        case AlgorithmMaximumIntensityProjection.X_PROJECTION:
                            maxAlgoClone[i].setStopSlice(image.getExtents()[0] - 1);
                            if (maxAlgoClone[i].getWindow() == -1
                                    || maxAlgoClone[i].getWindow() > image.getExtents()[0]) {
                                maxAlgoClone[i].setWindow(image.getExtents()[0]);
                            }
                            break;

                        case AlgorithmMaximumIntensityProjection.Y_PROJECTION:
                            maxAlgoClone[i].setStopSlice(image.getExtents()[1] - 1);
                            if (maxAlgoClone[i].getWindow() == -1
                                    || maxAlgoClone[i].getWindow() > image.getExtents()[1]) {
                                maxAlgoClone[i].setWindow(image.getExtents()[1]);
                            }
                            break;

                        case AlgorithmMaximumIntensityProjection.Z_PROJECTION:
                            maxAlgoClone[i].setStopSlice(image.getExtents()[2] - 1);
                            if (maxAlgoClone[i].getWindow() == -1
                                    || maxAlgoClone[i].getWindow() > image.getExtents()[2]) {
                                maxAlgoClone[i].setWindow(image.getExtents()[2]);
                            }
                            break;
                    }
                    maxAlgoClone[i].setRunningInSeparateThread(false);
                    maxAlgoClone[i].run();
                }

                String projFolder = "Proj";
                for (int i = 0; i < maxAlgoClone.length; i++) {
                    if (parentShow && showMaxProj) {
                        resultImageList.addAll(maxAlgoClone[i].getResultImage());
                    }

                    switch (maxAlgoClone[i].getProjectionDirection()) {
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

                    if (parentSave && saveMaxProj) {
                        Vector<ModelImage> resImageVec = maxAlgoClone[i].getResultImage();
                        for (int j = 0; j < resImageVec.size(); j++) {
                            options.setFileDirectory(parentDir.getAbsolutePath() + File.separator + projFolder
                                    + File.separator);
                            options.setFileName(resImageVec.get(j).getImageFileName());
                            options.setBeginSlice(0);
                            options.setOptionsSet(true);
                            options.setSaveAs(false);
                            if (resImageVec.get(j).getNDims() > 2) {
                                options.setEndSlice(resImageVec.get(j).getExtents()[2] - 1);
                            } else {
                                options.setEndSlice(0);
                            }
                            io.writeImage(resImageVec.get(j), options, false, allowScriptRecording);
                        }
                        if ( ! (parentShow && showMaxProj)) {
                            for (int j = resImageVec.size() - 1; j >= 0; j--) {
                                final ModelImage img = resImageVec.remove(j);
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
            final int[] dim = new int[transformImage.getExtents().length];
            final float[] res = new float[transformImage.getResolutions(0).length];
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
                    for (int i = 0; i < dim.length; i++) {
                        if (transformImage.getExtents()[i] > baseImage.getExtents()[i]) {
                            dim[i] = transformImage.getExtents()[i];
                        } else {
                            dim[i] = baseImage.getExtents()[i];
                        }
                    }

                    oXdim = dim[0];
                    oYdim = dim[1];
                    oZdim = dim[2];

                    for (int i = 0; i < res.length; i++) {
                        if (transformImage.getResolutions(0)[i] < baseImage.getResolutions(0)[i]) {
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

                    for (int i = 0; i < res.length; i++) {
                        if (transformImage.getResolutions(0)[i] == baseImage.getResolutions(0)[i]) {
                            res[i] = transformImage.getResolutions(0)[i];
                        } else if (transformImage.getResolutions(0)[i] > baseImage.getResolutions(0)[i]) {
                            res[i] = baseImage.getResolutions(0)[i] / transformImage.getResolutions(0)[i];
                        } else { // transform res < base res
                            res[i] = transformImage.getResolutions(0)[i] / baseImage.getResolutions(0)[i];
                        }
                    }

                    for (int i = 0; i < dim.length; i++) {
                        dim[i] = (int) ( (baseImage.getExtents()[i] + transformImage.getExtents()[i]) / 2.0);
                    }

                    oXdim = dim[0];
                    oYdim = dim[1];
                    oZdim = dim[2];
                    oXres = res[0];
                    oYres = res[1];
                    oZres = res[2];
                    break;

            }

            TransMatrix xfrm = new TransMatrix(4);
            if (registerAll) {
            	final int cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                final int DOF = 12;
                final int interp = AlgorithmTransform.TRILINEAR;
                final boolean maxResol = true;
                final boolean doSubsample = true;
                // Already in multithreads
                final boolean doMultiThread = false;
                final boolean fastMode = false;
                final int maxIterations = 2;
                final int numMinima = 3;
                AlgorithmRegOAR3D regAlgo3D = new AlgorithmRegOAR3D(baseImage, transformImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                        coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ,
                        coarseRateZ, fineRateZ, maxResol, doSubsample, doMultiThread, fastMode, maxIterations, numMinima);
                regAlgo3D.run();
                if (regAlgo3D.isCompleted()) {
                    final int xdimA = baseImage.getExtents()[0];
                    final int ydimA = baseImage.getExtents()[1];
                    final int zdimA = baseImage.getExtents()[2];
                    final float xresA = baseImage.getFileInfo(0).getResolutions()[0];
                    final float yresA = baseImage.getFileInfo(0).getResolutions()[1];
                    final float zresA = baseImage.getFileInfo(0).getResolutions()[2];
                    xfrm = regAlgo3D.getTransform();
                    // Do not do finalMatrix.Inverse()
                    xfrm.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                    final String costName = "CORRELATION_RATIO_SMOOTHED";
                    String message = "Using cost function, " + costName;
                    message += ", the cost is " + Double.toString(regAlgo3D.getAnswer()) + ".\n";
                    message += "Some registration settings: \n";
                    message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                    message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
                    message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                    message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
                    message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                    message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
                    mtxFileLoc = mtxFileDirectory + File.separator + transformImageName + "_To_" + baseImageName + ".mtx";
                    final int interp2 = AlgorithmTransform.TRILINEAR;
                    final boolean pad = false;
                    System.out.println("xfrm = " + xfrm);
                    xfrm.saveMatrix(mtxFileLoc, interp2, xresA, yresA, zresA, xdimA, ydimA, zdimA, true, false, pad,
                            message);
                    Preferences.debug("Saved " + mtxFileLoc + "\n", Preferences.DEBUG_FILEIO);
                    regAlgo3D.disposeLocal();
                    regAlgo3D = null;
                } // if (regAlgo3D.isCompleted())
                else {
                    MipavUtil.displayError("AlgorithmRegOAR3D did not complete successfully");
                    setCompleted(false);
                    return;
                }	
            } // if (registerAll)
            else { // !registerAll
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
	
	                final int fileInterp[] = new int[1];
	                final float fileXres[] = new float[1];
	                final float fileYres[] = new float[1];
	                final float fileZres[] = new float[1];
	                final int fileXdim[] = new int[1];
	                final int fileYdim[] = new int[1];
	                final int fileZdim[] = new int[1];
	                final boolean filetVOI[] = new boolean[1];
	                final boolean fileClip[] = new boolean[1];
	                final boolean filePad[] = new boolean[1];
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
            } // else !registerAll
            final int units[] = new int[3];
            units[0] = FileInfoBase.UNKNOWN_MEASURE;
            units[1] = FileInfoBase.UNKNOWN_MEASURE;
            units[2] = FileInfoBase.UNKNOWN_MEASURE;
            boolean doPad = false;
            final double fillValue = 0.0;
            int[]  margins = null;
            if (doPad) {
                margins = getImageMargins(transformImage, xfrm, oXres, oYres, oZres);  
                oXdim = oXdim + margins[0] + margins[3];
                oYdim = oYdim + margins[1] + margins[4];
                oZdim = oZdim + margins[2] + margins[5];
            }
            int iXdim = transformImage.getExtents()[0];
            int iYdim = transformImage.getExtents()[1];
            int iZdim = transformImage.getExtents()[2];
            float iXres = transformImage.getFileInfo()[0].getResolutions()[0];
            float iYres = transformImage.getFileInfo()[0].getResolutions()[1];
            float iZres = transformImage.getFileInfo()[0].getResolutions()[2];
            int extents[] = new int[3];
            extents[0] = oXdim;
            extents[1] = oYdim;
            extents[2] = oZdim;
            float[] destResolutions = new float[3];
            destResolutions[0] = oXres;
            destResolutions[1] = oYres;
            destResolutions[2] = oZres;
            TransMatrix transMatrix = xfrm.clone();
            TransMatrix xfrmInverse = matrixtoInverseArray(transMatrix);
            int imgLength = transformImage.getExtents()[0] * transformImage.getExtents()[1] * transformImage.getExtents()[2];
            Vector<TransMatrix> originalVector = transformImage.getMatrixHolder().getMatrices();
            double [] imgBuf = new double[imgLength];
            try {
                transformImage.exportData(0, imgLength, imgBuf);
            }
            catch (IOException e) {
            	e.printStackTrace();
            }
            final String name = transformImage.getImageName() + "_transform";
            int type = transformImage.getType();
            ModelImage transformImage2 = new ModelImage(type, extents, name);
            updateFileInfo(transformImage, transformImage2, destResolutions, units, transMatrix);
            transformImage.disposeLocal();
            transformImage = transformImage2;
            transformTrilinear(transformImage, imgBuf, xfrmInverse, iXdim, iYdim, iZdim,
            		iXres, iYres, iZres, oXdim, oYdim, oZdim, oXres, oYres, oZres,
            		doPad, margins, fillValue);
             // copy the src image's matrices into the destination image
            transformImage.getMatrixHolder().replaceMatrices(originalVector);

            // add the new transform matrix to the destination image
            transMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
            transformImage.getMatrixHolder().addMatrix(transMatrix);

            if (doInterImages) {
                new ViewJFrameImage(transformImage);
            }
        }

        private ModelImage subTransform(final ModelImage image, final TransMatrix mat, final int[] outDim,
                final float[] outRes) {
            final boolean doPad = true;
            final int units[] = new int[3];
            units[0] = FileInfoBase.UNKNOWN_MEASURE;
            units[1] = FileInfoBase.UNKNOWN_MEASURE;
            units[2] = FileInfoBase.UNKNOWN_MEASURE;
            final double fillValue = 0.0;
            int iXdim = image.getExtents()[0];
            int iYdim = image.getExtents()[1];
            int iZdim = image.getExtents()[2];
            float iXres = image.getFileInfo()[0].getResolutions()[0];
            float iYres = image.getFileInfo()[0].getResolutions()[1];
            float iZres = image.getFileInfo()[0].getResolutions()[2];
            int extents[] = new int[3];
            extents[0] = outDim[0];
            extents[1] = outDim[1];
            extents[2] = outDim[2];
            int[]  margins = null;
            if (doPad) {
                margins = getImageMargins(transformImage, mat, outRes[0], outRes[1], outRes[2]);  
                extents[0] = extents[0] + margins[0] + margins[3];
                extents[1] = extents[1] + margins[1] + margins[4];
                extents[2] = extents[2] + margins[2] + margins[5];
            }
            float[] destResolutions = new float[3];
            destResolutions[0] = outRes[0];
            destResolutions[1] = outRes[1];
            destResolutions[2] = outRes[2];
            final String name = image.getImageName() + "_transform";
            int type = image.getType();
            ModelImage subImage = new ModelImage(type, extents, name);
            TransMatrix transMatrix = mat.clone();
            TransMatrix xfrm = matrixtoInverseArray(transMatrix);
            int imgLength = iXdim * iYdim * iZdim;
            Vector<TransMatrix> originalVector = image.getMatrixHolder().getMatrices();
            double [] imgBuf = new double[imgLength];
            try {
                image.exportData(0, imgLength, imgBuf);
            }
            catch (IOException e) {
            	e.printStackTrace();
            }
            updateFileInfo(image, subImage, destResolutions, units, transMatrix);
            transformTrilinear(subImage, imgBuf, xfrm, iXdim, iYdim, iZdim, iXres, iYres, iZres,
            		extents[0], extents[1], extents[2], outRes[0], outRes[1], outRes[2],
            		doPad, margins, fillValue);
            if ( !doInterImages) {
                image.disposeLocal();
            }
             // copy the src image's matrices into the destination image
            subImage.getMatrixHolder().replaceMatrices(originalVector);

            // add the new transform matrix to the destination image
            transMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
            subImage.getMatrixHolder().addMatrix(transMatrix);

            return subImage;
        }
        
        /**
         * Transforms and resamples a 3D volume using trilinear interpolation.
         * 
         * @param imgBuffer Image array
         * @param kTM Transformation matrix to be applied
         */
        private void transformTrilinear(ModelImage destImage, final double[] imgBuffer, final TransMatrix kTM, 
        		int iXdim, int iYdim, int iZdim, 
        		float iXres, float iYres, float iZres, int oXdim, int oYdim, int oZdim, float oXres, float oYres, float oZres,
        		boolean pad, int [] margins, double fillValue) {
            int i, j, k;
            int iAdj, jAdj, kAdj;
            double[] tempBuf;
            double X, Y, Z;
            int x0, y0, z0;
            double value;
            double imm, jmm, kmm;
            double k1, k2, k3, j1, j2, j3;
            double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int deltaX, deltaY, deltaZ;

            int sliceSize;
            sliceSize = iXdim * iYdim;

            int imgLength2 = oXdim * oYdim * oZdim;
            tempBuf = new double[imgLength2];

            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;
            // T30 = (float)xfrm[3][0]; T31 = (float)xfrm[3][1]; T32 = (float)xfrm[3][2]; T33 = (float)xfrm[3][3];

            int position1, position2;
            double b1, b2;
            double dx, dy, dz, dx1, dy1;

            final double invXRes = 1.0 / iXres;
            final double invYRes = 1.0 / iYres;
            final double invZRes = 1.0 / iZres;

            int index = 0;

            for (k = 0; (k < oZdim); k++) {

                if (pad) {
                    kAdj = k - margins[2];
                } else {
                    kAdj = k;
                }

                kmm = kAdj * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim); j++) {

                    if (pad) {
                        jAdj = j - margins[1];
                    } else {
                        jAdj = j;
                    }

                    jmm = jAdj * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim); i++) {

                        // transform i,j,k
                        value = fillValue; // if voxel transforms out of bounds
                        if (pad) {
                            iAdj = i - margins[0];
                        } else {
                            iAdj = i;
                        }

                        imm = iAdj * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ( (X > -0.5) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;
                            if ( (Y > -0.5) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;
                                if ( (Z > -0.5) && (Z < iZdim)) {

                                    if (X <= 0) {
                                        x0 = 0;
                                        dx = 0;
                                        deltaX = 0;
                                    } else if (X >= (iXdim - 1)) {
                                        x0 = iXdim - 1;
                                        dx = 0;
                                        deltaX = 0;
                                    } else {
                                        x0 = (int) X;
                                        dx = X - x0;
                                        deltaX = 1;
                                    }

                                    if (Y <= 0) {
                                        y0 = 0;
                                        dy = 0;
                                        deltaY = 0;
                                    } else if (Y >= (iYdim - 1)) {
                                        y0 = iYdim - 1;
                                        dy = 0;
                                        deltaY = 0;
                                    } else {
                                        y0 = (int) Y;
                                        dy = Y - y0;
                                        deltaY = iXdim;
                                    }

                                    if (Z <= 0) {
                                        z0 = 0;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else if (Z >= (iZdim - 1)) {
                                        z0 = iZdim - 1;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else {
                                        z0 = (int) Z;
                                        dz = Z - z0;
                                        deltaZ = sliceSize;
                                    }

                                    dx1 = 1 - dx;
                                    dy1 = 1 - dy;

                                    position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                    position2 = position1 + deltaZ;

                                    b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                            + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                    + deltaY + deltaX])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                            + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                    + deltaY + deltaX])));

                                    value = ( (1 - dz) * b1) + (dz * b2);

                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds

                        tempBuf[index++] = value;
                    } // end for i
                } // end for j
            } // end for k

            try {
                destImage.importData(0, tempBuf, true);
                tempBuf = null;
            } catch (final IOException error) {
                error.printStackTrace();
            }
        }
        
        /**
         * Calculate necessary padding for image given applied transform.
         * 
         * @param srcImage DOCUMENT ME!
         * @param transMatrix array with transformation matrix
         * @param dxOut DOCUMENT ME!
         * @param dyOut DOCUMENT ME!
         * @param dzOut DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        private int[] getImageMargins(final ModelImage srcImage, final TransMatrix transMatrix, final float dxOut,
                final float dyOut, final float dzOut) {
        	int margins[] = new int[6];
            int i;
            final float xi = 0.f;
            final float yi = 0.f;
            final float zi = 0.f;
            final float dx = srcImage.getFileInfo(0).getResolutions()[0];
            final float dy = srcImage.getFileInfo(0).getResolutions()[1];
            final float dz = srcImage.getFileInfo(0).getResolutions()[2];
            final int nx = srcImage.getExtents()[0];
            final int ny = srcImage.getExtents()[1];
            final int nz = srcImage.getExtents()[2];
            float xf = 0.f, yf = 0.f, zf = 0.f;
            float minx, miny, maxx, maxy, minz, maxz;
            int leftPad = 0, rightPad = 0, topPad = 0, bottomPad = 0, front = 0, back = 0;
            Vector3f[] ptsi3, ptsf3;

            /* Set the far corner of the image volume in mm (but relative to image origin, in image coordinates). */
            xf = xi + (dx * (nx - 1));
            minx = xi;
            maxx = xf;
            yf = yi + (dy * (ny - 1));
            miny = yi;
            maxy = yf;
            zf = zi + (dz * (nz - 1));
            minz = zi;
            maxz = zf;
            // System.out.println("Far corner: " +(int)xf +", " +(int)yf +", " +(int)zf);

            /*
             * Set up array of 8 points representing the corners of the image volume and then transform them with
             * transMatrix.
             */
            ptsi3 = new Vector3f[8];
            ptsf3 = new Vector3f[8];

            for (i = 1; i <= 8; i++) {
                ptsi3[i - 1] = new Vector3f();
                ptsf3[i - 1] = new Vector3f();

                if ( (i == 1) || (i == 4) || (i == 5) || (i == 8)) {
                    ptsi3[i - 1].X = xi;
                } else {
                    ptsi3[i - 1].X = xf;
                }

                if ( (i == 1) || (i == 2) || (i == 5) || (i == 6)) {
                    ptsi3[i - 1].Y = yi;
                } else {
                    ptsi3[i - 1].Y = yf;
                }

                if ( (i == 1) || (i == 2) || (i == 3) || (i == 4)) {
                    ptsi3[i - 1].Z = zi;
                } else {
                    ptsi3[i - 1].Z = zf;
                }
                // System.out.println("Initial point " +i +": " +(int)ptsi3[i-1].X +", " +(int)ptsi3[i-1].Y +", "
                // +(int)ptsi3[i-1].Z);
            }

            /* Transform corner points, ptsi3, to get transformed points, ptsf3. */
            for (i = 1; i <= 8; i++) {
                transMatrix.transformAsPoint3Df(ptsi3[i - 1], ptsf3[i - 1]);
            }

            /* Find new min and max values for the transformed point. */
            for (i = 1; i <= 8; i++) {

                // System.out.println("Transformed point " +i +": " +(int)ptsf3[i-1].X +", " +(int)ptsf3[i-1].Y +", "
                // +(int)ptsf3[i-1].Z);
                if (ptsf3[i - 1].X < minx) {
                    minx = ptsf3[i - 1].X;
                }

                if (ptsf3[i - 1].X > maxx) {
                    maxx = ptsf3[i - 1].X;
                }

                if (ptsf3[i - 1].Y < miny) {
                    miny = ptsf3[i - 1].Y;
                }

                if (ptsf3[i - 1].Y > maxy) {
                    maxy = ptsf3[i - 1].Y;
                }

                if (ptsf3[i - 1].Z < minz) {
                    minz = ptsf3[i - 1].Z;
                }

                if (ptsf3[i - 1].Z > maxz) {
                    maxz = ptsf3[i - 1].Z;
                }
            }
            // System.out.println("Bounding box first corner: " +(int)minx +", " +(int)miny +", " +(int)minz);
            // System.out.println("Bounding box far corner: " +(int)maxx +", " +(int)maxy +", " +(int)maxz);

            /* Calculate padding. */
            leftPad = (int) ( ( (xi - minx) / dxOut) + 0.5);
            rightPad = (int) ( ( (maxx - xf) / dxOut) + 0.5);

            // System.out.println("Padding in x is: " + leftPad +" and " +rightPad);
            topPad = (int) ( ( (yi - miny) / dyOut) + 0.5);
            bottomPad = (int) ( ( (maxy - yf) / dyOut) + 0.5);

            // System.out.println("Padding in y is: " + topPad+" and " +bottomPad);
            front = (int) ( ( (zi - minz) / dzOut) + 0.5);
            back = (int) ( ( (maxz - zf) / dzOut) + 0.5);
            // System.out.println("Padding in z is: " + front + " and " + back);

            margins[0] = leftPad;
            margins[1] = topPad;
            margins[2] = front;
            margins[3] = rightPad;
            margins[4] = bottomPad;
            margins[5] = back;

            return margins;
        }
        
        /**
         * Converts matrix to inverse array.
         * 
         * @param transMatrix Matrix to convert.
         * 
         * @return The inverted array.
         */
        private final TransMatrix matrixtoInverseArray(final TransMatrix transMatrix) {

            if (transMatrix.isIdentity()) {

                // Added explicit handling of identity matrix - or else the new matrix is other than
                // identity because of round-off error. This situation (of identity) matrix
                // occurs frequently -- any time there is resampling without transformation.
                return new TransMatrix(transMatrix);
            } else {
                final TransMatrix kTM = new TransMatrix(transMatrix);
                kTM.Inverse();
                return kTM;
            }

        }
        
        /**
         * Copy important file information to resultant image structure.
         * 
         * @param image Source image.
         * @param resultImage Resultant image.
         * @param resolutions DOCUMENT ME!
         * @param units DOCUMENT ME!
         * @param matrix DOCUMENT ME!
         * @param useSATransform DOCUMENT ME!
         * @param m
         */
        private void updateFileInfo(final ModelImage image, final ModelImage resultImage, final float[] resolutions,
                final int[] units, final TransMatrix matrix) {

            final FileInfoBase[] fileInfo = resultImage.getFileInfo();
            float firstPos[] = null;
            float delPos[] = null;
            float sliceLocation0 = Float.NaN;
            float delLoc = Float.NaN;

            if (resultImage.getNDims() == 2) {
                fileInfo[0] = (FileInfoBase) image.getFileInfo(0).clone();
                fileInfo[0].setDataType(resultImage.getType());
                fileInfo[0].setModality(image.getFileInfo()[0].getModality());
                fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
                fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
                fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[0].setResolutions(resolutions);
                fileInfo[0].setExtents(resultImage.getExtents());
                fileInfo[0].setMax(resultImage.getMax());
                fileInfo[0].setMin(resultImage.getMin());
                fileInfo[0].setImageOrientation(image.getImageOrientation());
                fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
                fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
                fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
                fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
                fileInfo[0].setUnitsOfMeasure(units);

            } else if (resultImage.getNDims() == 3) {
                final float[] coord = new float[3];
                final float[] tempPos = new float[3];
                String orientation;

                if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                    final FileInfoDicom oldDicomInfo = (FileInfoDicom) image.getFileInfo(0);
                    final FileDicomTagTable[] childTagTables = new FileDicomTagTable[resultImage.getExtents()[2] - 1];

                    // first create all of the new file infos (reference and children) and fill them with tags from the old
                    // file info. some of these tag values will be overridden in the next loop
                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                        if (i == 0) {

                            // create a new reference file info
                            fileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                    oldDicomInfo.getFileFormat());
                            ((FileInfoDicom) fileInfo[0]).setVr_type(oldDicomInfo.getVr_type());
                        } else {

                            // all other slices are children of the first file info..
                            fileInfo[i] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                    oldDicomInfo.getFileFormat(), (FileInfoDicom) fileInfo[0]);
                            ((FileInfoDicom) fileInfo[i]).setVr_type(oldDicomInfo.getVr_type());
                            childTagTables[i - 1] = ((FileInfoDicom) fileInfo[i]).getTagTable();
                        }

                        if (image.getExtents()[2] > i) {

                            // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                            ((FileInfoDicom) fileInfo[i]).getTagTable().importTags((FileInfoDicom) image.getFileInfo(i));
                        } else {

                            // not possible for other rotations because the z-dimension is different
                            ((FileInfoDicom) fileInfo[i]).getTagTable().importTags(oldDicomInfo);
                        }
                        ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0028,0010",
                                new Short((short) resultImage.getExtents()[1]), 2);
                        ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0028,0011",
                                new Short((short) resultImage.getExtents()[0]), 2);
                    }

                    ((FileInfoDicom) fileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
                } else {

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                        if (image.getExtents()[2] > i) {
                            fileInfo[i] = (FileInfoBase) image.getFileInfo(i).clone();
                        } else {
                            fileInfo[i] = (FileInfoBase) image.getFileInfo(0).clone();
                        }

                    }
                }

                if ( (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM)
                        && (image.getExtents()[2] != resultImage.getExtents()[2])) {

                    float lastPos[] = null;
                    orientation = (String) ((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0020,0032");
                    if (orientation != null) {

                        int index1 = -1, index2 = -1;

                        for (int k = 0; k < orientation.length(); k++) {

                            if (orientation.charAt(k) == '\\') {

                                if (index1 == -1) {
                                    index1 = k;
                                } else {
                                    index2 = k;
                                }
                            }
                        }

                        coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                        coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                        coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();
                        firstPos = new float[3];

                        matrix.transform(coord[0], coord[1], coord[2], firstPos);
                    } // if (orientation != null)
                    orientation = (String) ((FileInfoDicom) fileInfo[resultImage.getExtents()[2] - 1]).getTagTable()
                            .getValue("0020,0032");

                    if (orientation != null) {

                        int index1 = -1, index2 = -1;

                        for (int k = 0; k < orientation.length(); k++) {

                            if (orientation.charAt(k) == '\\') {

                                if (index1 == -1) {
                                    index1 = k;
                                } else {
                                    index2 = k;
                                }
                            }
                        }

                        coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                        coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                        coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();

                        lastPos = new float[3];

                        matrix.transform(coord[0], coord[1], coord[2], lastPos);
                    } // if (orientation != null)
                    if ( (firstPos != null) && (lastPos != null)) {
                        delPos = new float[3];
                        for (int i = 0; i <= 2; i++) {
                            delPos[i] = (lastPos[i] - firstPos[i]) / (resultImage.getExtents()[2] - 1);
                        }
                    } // if ((firstPos != null) && (lastPos != null)
                    if ( ( ((FileInfoDicom) fileInfo[0]).getTagTable().containsTag("0020,1041"))
                            && ( ((FileInfoDicom) fileInfo[1]).getTagTable().containsTag("0020,1041"))) {
                        if ( ((String) ((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0020,1041") != null)
                                && ((String) ((FileInfoDicom) fileInfo[1]).getTagTable().getValue("0020,1041") != null)) {
                            try {
                                sliceLocation0 = Float.parseFloat((String) ((FileInfoDicom) fileInfo[0]).getTagTable()
                                        .getValue("0020,1041"));
                                final float sliceLocation1 = Float.parseFloat((String) ((FileInfoDicom) fileInfo[1])
                                        .getTagTable().getValue("0020,1041"));
                                delLoc = (sliceLocation1 - sliceLocation0) * resolutions[2]
                                        / image.getFileInfo()[0].getResolutions()[2];
                            } catch (final NumberFormatException nfe) {

                            }
                        }

                    }
                } // if ((image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) &&

                for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                    fileInfo[i].setDataType(resultImage.getType());
                    fileInfo[i].setResolutions(resolutions);
                    fileInfo[i].setSliceThickness(resolutions[2]);
                    fileInfo[i].setExtents(resultImage.getExtents());
                    fileInfo[i].setMax(resultImage.getMax());
                    fileInfo[i].setMin(resultImage.getMin());
                    fileInfo[i].setUnitsOfMeasure(units);

                    if (fileInfo[i].getFileFormat() == FileUtility.DICOM) {
                        if (image.getExtents()[2] == resultImage.getExtents()[2]) {
                            // don't interpolate here in case spacing between slices is uneven
                            orientation = (String) ((FileInfoDicom) fileInfo[i]).getTagTable().getValue("0020,0032");

                            if (orientation != null) {

                                int index1 = -1, index2 = -1;

                                for (int k = 0; k < orientation.length(); k++) {

                                    if (orientation.charAt(k) == '\\') {

                                        if (index1 == -1) {
                                            index1 = k;
                                        } else {
                                            index2 = k;
                                        }
                                    }
                                }

                                coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                                coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                                coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();

                                matrix.transform(coord[0], coord[1], coord[2], tempPos);

                                // System.err.println("transformed " + orientation + " to: " +tempPos[0] + " " + tempPos[1]
                                // + "
                                // " + tempPos[2]);
                                orientation = tempPos[0] + "\\" + tempPos[1] + "\\" + tempPos[2];
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0032", orientation);
                            } // if (orientation != null)

                        } // if (image.getExtents()[2] == resultImage.getExtents()[2])
                        else { // image.getExtents()[2] != resultImage.getExtents()[2]
                            if (delPos != null) {
                                orientation = (firstPos[0] + i * delPos[0]) + "\\" + (firstPos[1] + i * delPos[1]) + "\\"
                                        + (firstPos[2] + i * delPos[2]);
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0032", orientation);
                            } // if (delPos != null)
                            if ( !Float.isNaN(delLoc)) {
                                final String sliceLoc = Float.toString(sliceLocation0 + i * delLoc);
                                // slice location
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,1041", sliceLoc,
                                        sliceLoc.length());
                            }
                            final String instanceString = Integer.toString(i + 1);
                            ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0013", instanceString,
                                    instanceString.length());
                            final String imagesInAcquisition = Integer.toString(resultImage.getExtents()[2]);
                            ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,1002", imagesInAcquisition,
                                    imagesInAcquisition.length());
                            final String res2 = String.valueOf(resolutions[2]);
                            if ( ((FileInfoDicom) fileInfo[i]).getTagTable().containsTag("0018,0050")) {
                                // Slice thickness
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0018,0050", res2, res2.length());
                            }
                            if ( ((FileInfoDicom) fileInfo[i]).getTagTable().containsTag("0018,0088")) {
                                // Spacing between slices
                                ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0018,0088", res2, res2.length());
                            }
                        } // else image.getExtents()[2] != resultImage.getExtents()[2]
                    } // if (fileInfo[i].getFileFormat() == FileUtility.DICOM)
                } // for (int i = 0; i < resultImage.getExtents()[2]; i++)
            } 

            resultImage.setFileInfo(fileInfo);
        }

        private void downsampleUpsampleCombined() {

            final TransMatrix mat = new TransMatrix(4);
            mat.identity();
            mat.set(0, 0, baseImage.getResolutions(0)[0] / transformImage.getResolutions(0)[0]);

            mat.set(2, 2, baseImage.getResolutions(0)[2] / transformImage.getResolutions(0)[2]);

            baseImage = subTransform(baseImage, mat, baseImage.getExtents(), baseImage.getResolutions(0));

            baseImage.setResolutions(new float[] {transformImage.getResolutions(0)[0],
                    transformImage.getResolutions(0)[1], transformImage.getResolutions(0)[2]});
            for (int i = 0; i < baseImage.getResolutions(0).length; i++) {
                finalRes[i] = baseImage.getResolutions(0)[i];
            }
            for (int i = 0; i < baseImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            if (doInterImages) {
                new ViewJFrameImage(baseImage);
            }
        }

        private void upsampleToTransform() {
            final TransMatrix mat = new TransMatrix(4);
            mat.identity();
            mat.set(0, 0, baseImage.getResolutions(0)[0] / transformImage.getResolutions(0)[0]);
            mat.set(2, 2, baseImage.getResolutions(0)[2] / transformImage.getResolutions(0)[2]);

            baseImage = subTransform(baseImage, mat, baseImage.getExtents(), baseImage.getResolutions(0));

            for (int i = 0; i < baseImage.getResolutions(0).length; i++) {
                finalRes[i] = transformImage.getResolutions(0)[i];
            }
            for (int i = 0; i < baseImage.getFileInfo().length && i < transformImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(transformImage.getResolutions(i)[2]);
            }
            if (doInterImages) {
                new ViewJFrameImage(baseImage);
            }
        }

        private ModelImage rotate(ModelImage image, final int mode) {
            final AlgorithmRotate rotate = new AlgorithmRotate(image, mode);
            rotate.setQuiet(true);
            rotate.run(); // transform image replaced
            image.disposeLocal();
            image = rotate.getDestImage();
            rotate.finalize();
            if (doInterImages) {
                new ViewJFrameImage(image);
            }

            return image;

        }

        private void calcGeoMean() {

            int transformX, transformY, transformZ;
            double baseVal = 0, transVal = 0;
            double mult = 0;
            // new ViewJFrameImage(transformImage);
            for (int i = 0; i < subGeoImage.getExtents()[0]; i++) {
                transformX = i - xMovement;
                for (int j = 0; j < subGeoImage.getExtents()[1]; j++) {
                    transformY = j - yMovement;
                    for (int k = 0; k < subGeoImage.getExtents()[2]; k++) {
                        mult = 0;
                        baseVal = 0;
                        transVal = 0;
                        transformZ = k - zMovement;
                        if (transformX >= 0 && transformX < transformImage.getExtents()[0] && transformY >= 0
                                && transformY < transformImage.getExtents()[1] && transformZ >= 0
                                && transformZ < transformImage.getExtents()[2]) {
                            transVal = transformImage.getDouble(transformX, transformY, transformZ);
                            mult += transformGeoWeight;
                        }
                        if (i < baseImage.getExtents()[0] && j < baseImage.getExtents()[1]
                                && k < baseImage.getExtents()[2]) {
                            baseVal = baseImage.getDouble(i, j, k);
                            mult += baseGeoWeight;
                        }

                        subGeoImage.set(
                                i,
                                j,
                                k,
                                Math.exp( (transformGeoWeight * Math.log(transVal) + baseGeoWeight * Math.log(baseVal))
                                        / mult));
                    }
                }
            }

            Preferences.data("Produced weighted geometric mean: Transformed weight: " + transformGeoWeight
                    + "\tBase weight: " + baseGeoWeight + "\n");

            subGeoImage.calcMinMax();

            subGeoImage.setImageName("GeoMeanFused_" + baseImageName);

            if (doInterImages) {
                new ViewJFrameImage(subGeoImage);
            }
        }

        private void calcAriMean() {

            int transformX, transformY, transformZ;
            double baseVal = 0, transVal = 0;
            double mult = 0;
            for (int i = 0; i < subAriImage.getExtents()[0]; i++) {
                transformX = i - xMovement;
                for (int j = 0; j < subAriImage.getExtents()[1]; j++) {
                    transformY = j - yMovement;
                    for (int k = 0; k < subAriImage.getExtents()[2]; k++) {
                        mult = 0;
                        baseVal = 0;
                        transVal = 0;
                        transformZ = k - zMovement;
                        if (transformX >= 0 && transformX < transformImage.getExtents()[0] && transformY >= 0
                                && transformY < transformImage.getExtents()[1] && transformZ >= 0
                                && transformZ < transformImage.getExtents()[2]) {
                            transVal = transformImage.getDouble(transformX, transformY, transformZ);
                            mult += transformAriWeight;
                        }
                        if (i < baseImage.getExtents()[0] && j < baseImage.getExtents()[1]
                                && k < baseImage.getExtents()[2]) {
                            baseVal = baseImage.getDouble(i, j, k);
                            mult += baseAriWeight;
                        }
                        if (mult == 0) {
                            subAriImage.set(i, j, k, 0);
                        } else {
                            subAriImage.set(i, j, k, (transVal * transformAriWeight + baseVal * baseAriWeight) / mult);
                        }
                    }
                }
            }

            Preferences.data("Produced weighted arithmetic mean: Transformed weight: " + transformAriWeight
                    + "\tBase weight: " + baseAriWeight + "\n");

            subAriImage.calcMinMax();

            subAriImage.setImageName("AriMeanFused_" + baseImageName);

            if (doInterImages) {
                new ViewJFrameImage(subAriImage);
            }
        }

        /**
         * Performs deconvolution to combine two provided images. Adds the deconvolved image to the result image list.
         * 
         * @param imageA The base image.
         * @param imageB The transform image.
         * @return The deconvolved image.
         */
        private ModelImage deconvolve(final ModelImage imageA, final ModelImage imageB) {
            final int index = baseImageName.lastIndexOf("_");
            String name;
            if (index != -1) {
                name = "Decon" + baseImageName.substring(index);
            } else {
                name = "Decon_" + baseImageName;
            }

            final ModelImage resultImage = ViewUserInterface.getReference().createBlankImage(
                    (FileInfoBase) imageA.getFileInfo(0).clone(), false, allowScriptRecording);
            resultImage.setImageName(name);
            for (int i = 0; i < imageA.getFileInfo().length; i++) {
            	for (int j = 0; j < 3; j++) {
            	    resultImage.getFileInfo()[i].setUnitsOfMeasure(FileInfoBase.UNKNOWN_MEASURE, j);
            	}
            }

            if (deconvPlatform == OpenCLPlatform) {
	            final OpenCLAlgorithmDeconvolution deconvAlgo = new OpenCLAlgorithmDeconvolution(resultImage, imageA,
	                    imageB, deconvolutionMethod, deconvSigmaA, deconvSigmaB, true, deconvIterations, useDeconvSigmaConversionFactor);
	            deconvAlgo.setRed(true);
	            deconvAlgo.setGreen(true);
	            deconvAlgo.setBlue(true);
	
	            // can only perform one deconvolution at a time because it uses the GPU
	            synchronized (parentObject) {
	                deconvAlgo.run();
	            }
	
	            if (deconvShowResults) {
	                resultImageList.add(deconvAlgo.getDestImage());
	            }
	
	            return deconvAlgo.getDestImage();
            } // if (deconvPlatform == OpenCLPlatform)
            else {
            	deconvolutionSep3D_Dual(imageA, imageB, resultImage);
            	if (deconvShowResults) {
            		resultImageList.add(resultImage);
            	}
            	
            	return resultImage;
            }
        }
        
        private void deconvolutionSep3D_Dual(ModelImage imageA, ModelImage imageB, ModelImage resultImage) {
        	int i;
        	int iter;
        	float currentMax;
        	float originalMax;
        	float scale;
            int width = imageA.getExtents()[0];
            int height = imageA.getExtents()[1];
            int depth = imageA.getExtents()[2];
            int elementCount = width * height * depth;
            float[] inputA = new float[ elementCount ];
            try {
            	imageA.exportData(0, elementCount, inputA);
            }
            catch (IOException e) {
    			e.printStackTrace();
    		}
            float[] inputB = new float[ elementCount ];
            try {
            	imageB.exportData(0, elementCount, inputB);
            }
            catch (IOException e) {
    			e.printStackTrace();
    		}
            float estimateBuffer[] = new float[elementCount];
            float estimateBufferA[] = null;
            float estimateBufferB[] = null;
            if ((deconvolutionMethod == AVERAGE_DECON) || (deconvolutionMethod == MULTIPLICATION_DECON)) {
                estimateBufferA = new float[elementCount];
                estimateBufferB = new float[elementCount];
            }
            float blurredBuffer[] = new float[elementCount];
            float tempBuffer[] = new float[elementCount];
            originalMax = -Float.MAX_VALUE;
            for (i = 0; i < elementCount; i++) {
            	estimateBuffer[i] = 0.5f * (inputA[i] + inputB[i]);
            	if (estimateBuffer[i] > originalMax) {
            		originalMax = estimateBuffer[i];
            	}
            }
            float derivativeX[][] = new float[2][];
            float derivativeY[][] = new float[2][];
            float derivativeZ[][] = new float[2][];
            int kExtents[][] = new int[2][];
            int kOrigins[][] = new int[2][];
            initConvolutionBuffers(0, deconvSigmaA, derivativeX, derivativeY, derivativeZ, kExtents, kOrigins);
    		initConvolutionBuffers(1, deconvSigmaB, derivativeX, derivativeY, derivativeZ, kExtents, kOrigins);
              
            // Iterate over the algorithm:
    		for (iter = 0; iter < deconvIterations; iter++ )
    		{
    			// 
    			//  Compute:
    	        //  estimate *= blur(data_A / blur(estimate, view='a'), view='a')
    			//
    			//convolve X:
    			for (i = 0; i < depth; i++ )
    			{
    			    convolveX(estimateBuffer, derivativeX[0], blurredBuffer, width, height, kExtents[0][0],
    			    		kOrigins[0][0], i);	
    			}
    			
    			//convolve Y:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveY(blurredBuffer, derivativeY[0], tempBuffer, width, height, kExtents[0][1],
    			    		kOrigins[0][1], i);		
    			}
    			
    			//convolve Z and divide the original image by the result:
    			for (i = 0; i < depth; i++ )
    			{
    			    convolveZDiv(tempBuffer, derivativeZ[0], blurredBuffer, inputA,
    			    		width, height, depth, kExtents[0][2], kOrigins[0][2], 0, i);
    			}
    			
    			// 2nd blur and multiply result into estimate:
    			//convolve X:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveX(blurredBuffer, derivativeX[0], tempBuffer, width, height, kExtents[0][0],
    			    		kOrigins[0][0], i);		
    			}
    			
    			//convolve Y:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveY(tempBuffer, derivativeY[0], blurredBuffer, width, height, kExtents[0][1],
    			    		kOrigins[0][1], i);		
    			}
    			
    			if (deconvolutionMethod == JOINT_DECON) {
    				//convolve Z and multiply back into the estimate:
    				for (i = 0; i < depth; i++ )
    				{
    					convolveZMult(blurredBuffer, derivativeZ[0], estimateBuffer, estimateBuffer,
        			    		width, height, depth, kExtents[0][2], kOrigins[0][2], 0, i);	
    				}
    			}
    			else { // method == AVERAGE_DECON OR MULTIPLICATION_DECON
    				//convolve Z
    				for (i = 0; i < depth; i++ )
    				{
    					convolveZ(blurredBuffer, derivativeZ[0], estimateBufferA,
        			    		width, height, depth, kExtents[0][2], kOrigins[0][2], 0, i);		
    				}	
    			} // else method == AVERAGE_DECON OR MULTIPLICATION_DECON
    			
    			// 
    			//  Compute:
    	        //  estimate *= blur(data_B / blur(estimate, view='b'), view='b')
    			//
    			//convolve X:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveX(estimateBuffer, derivativeX[1], blurredBuffer, width, height, kExtents[1][0],
    			    		kOrigins[1][0], i);		
    			}
    			
    			//convolve Y:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveY(blurredBuffer, derivativeY[1], tempBuffer, width, height, kExtents[1][1],
    			    		kOrigins[1][1], i);		
    			}
    			
    			//convolve Z and divide the original image by the result:
    			for (i = 0; i < depth; i++ )
    			{
    			    convolveZDiv(tempBuffer, derivativeZ[1], blurredBuffer, inputB,
    			    		width, height, depth, kExtents[1][2], kOrigins[1][2], 0, i);
    			}
    			
    			// 2nd blur and multiply result into estimate:
    			//convolve X:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveX(blurredBuffer, derivativeX[1], tempBuffer, width, height, kExtents[1][0],
    			    		kOrigins[1][0], i);			
    			}
    			
    			//convolve Y:
    			for (i = 0; i < depth; i++ )
    			{
    				convolveY(tempBuffer, derivativeY[1], blurredBuffer, width, height, kExtents[1][1],
    			    		kOrigins[1][1], i);			
    			}
    			
    			if (deconvolutionMethod == JOINT_DECON) {
    				//convolve Z and multiply back into the estimate:
    				for (i = 0; i < depth; i++ )
    				{
    					convolveZMult(blurredBuffer, derivativeZ[1], estimateBuffer, estimateBuffer,
        			    		width, height, depth, kExtents[1][2], kOrigins[1][2], 0, i);	
    				}
    			}
    			else { // method == AVERAGE_DECON OR MULTIPLICATION_DECON
    				//convolve Z
    				for (i = 0; i < depth; i++ )
    				{
    					convolveZ(blurredBuffer, derivativeZ[1], estimateBufferB,
        			    		width, height, depth, kExtents[1][2], kOrigins[1][2], 0, i);		
    				}	
    			} // else method == AVERAGE_DECON OR MULTIPLICATION_DECON
    			
    			if (deconvolutionMethod == AVERAGE_DECON) {
    			    for (i = 0; i < elementCount; i++) {
    			    	estimateBuffer[i] = 0.5f * estimateBuffer[i]*(estimateBufferA[i] + estimateBufferB[i]);
    			    }
    			}
    			else if (deconvolutionMethod == MULTIPLICATION_DECON) {
    				for (i = 0; i < elementCount; i++) {
    			    	estimateBuffer[i] = (float)(estimateBuffer[i]*Math.sqrt(estimateBufferA[i] * estimateBufferB[i]));
    			    }	
    			}
    			
    			currentMax = -Float.MAX_VALUE;
    			for (i = 0; i < elementCount; i++) {
    				if (estimateBuffer[i] > currentMax) {
    					currentMax = estimateBuffer[i];
    				}
    			}
				scale = originalMax/currentMax;
				for (i = 0; i < elementCount; i++) {
					estimateBuffer[i] *= scale;
				}
    		} // for (iter = 0; iter < deconvIterations; iter++ )
    		
    		try {
    			resultImage.importData(0, estimateBuffer, true);
    		}
    		catch(IOException e) {
    		    e.printStackTrace();	
    		}
        }
        
        private void convolveX(float input[], float dx[], float output[], int xDim, int yDim, 
        		int maskSize, int maskOrigin, int slice) {
        	int length = xDim * yDim;
        	int offset = slice * length;
        	int x;
        	int y;
        	int mx;
        	int ix;
        	int i;
        	int index;
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			float sumX = 0.0f;
        			float dxVal = 0.0f;
        			for (mx = 0; mx < maskSize; mx++) {
        			    ix = x - maskOrigin + mx;
        			    i = offset + y * xDim + ix;
        			    if ((ix >= 0) && (ix < xDim)) {
        			        dxVal = dx[mx];
        			        sumX += input[i] * dxVal;
        			    }
        			} // for (mx = 0; mx < maskSize; mx++)
        			index = offset + y * xDim + x;
        			output[index] = sumX;
        			if (maskSize == 0) {
        				output[index] = input[index];
        			}
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
        }
        
        private void convolveY(float input[], float dy[], float output[], int xDim, int yDim, 
        		int maskSize, int maskOrigin, int slice) {
        	int length = xDim * yDim;
        	int offset = slice * length;
        	int x;
        	int y;
        	int my;
        	int iy;
        	int i;
        	int index;
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			float sumY = 0.0f;
        			float dyVal = 0.0f;
        			for (my = 0; my < maskSize; my++) {
        			    iy = y - maskOrigin + my;
        			    i = offset + iy * xDim + x;
        			    if ((iy >= 0) && (iy < yDim)) {
        			        dyVal = dy[my];
        			        sumY += input[i] * dyVal;
        			    }
        			} // for (my = 0; my < maskSize; my++)
        			index = offset + y * xDim + x;
        			output[index] = sumY;
        			if (maskSize == 0) {
        				output[index] = input[index];
        			}
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
        }
        
        private void convolveZ(float input[], float dz[], float output[],
        		int xDim, int yDim, int zDim, 
        		int maskSize, int maskOrigin, int clipZ, int slice) {
        	int length = xDim * yDim;
        	int offset = slice * length;
        	int x;
        	int y;
        	int mz;
        	int iz;
        	int i;
        	int index;
        	if ((clipZ != 0) && ((slice < (maskSize/2)) || (slice >= (zDim - (maskSize/2))))) {
        	    for (i = 0; i < length; i++) {
        	    	output[offset + i] = input[offset + i];
        	    }
        	    return;
        	} // if ((clipZ != 0) && ((slice < (maskSize/2)) || (slice >= (zDim - (maskSize/2)))))
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			float sumZ = 0.0f;
        			float dzVal = 0.0f;
        			for (mz = 0; mz < maskSize; mz++) {
        			    iz = slice - maskOrigin + mz ;
        			    i = iz * length + y * xDim + x;
        			    if ((iz >= 0) && (iz < zDim)) {
        			        dzVal = dz[mz];
        			        sumZ += input[i] * dzVal;
        			    }
        			} // for (my = 0; my < maskSize; my++)
        			index = offset + y * xDim + x;
        		    output[index] = sumZ;
        		    if (maskSize == 0) {
        		    	output[index] = input[index];
        		    }
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
        }
        
        private void convolveZDiv(float input[], float dz[], float output[], float original[],
        		int xDim, int yDim, int zDim, 
        		int maskSize, int maskOrigin, int clipZ, int slice) {
        	int length = xDim * yDim;
        	int offset = slice * length;
        	int x;
        	int y;
        	int mz;
        	int iz;
        	int i;
        	int index;
        	if ((clipZ != 0) && ((slice < (maskSize/2)) || (slice >= (zDim - (maskSize/2))))) {
        	    for (i = 0; i < length; i++) {
        	    	output[offset + i] = original[offset + i]/input[offset + i];
        	    }
        	    return;
        	} // if ((clipZ != 0) && ((slice < (maskSize/2)) || (slice >= (zDim - (maskSize/2)))))
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			float sumZ = 0.0f;
        			float dzVal = 0.0f;
        			for (mz = 0; mz < maskSize; mz++) {
        			    iz = slice - maskOrigin + mz ;
        			    i = iz * length + y * xDim + x;
        			    if ((iz >= 0) && (iz < zDim)) {
        			        dzVal = dz[mz];
        			        sumZ += input[i] * dzVal;
        			    }
        			} // for (my = 0; my < maskSize; my++)
        			index = offset + y * xDim + x;
        			if (sumZ != 0.0f) {
        				output[index] = original[index]/sumZ;
        			}
        			else {
        				output[index] = 0.0f;
        			}
        			if (maskSize == 0) {
        				output[index] = original[index]/input[index];
        			}
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
        }
        
        private void convolveZMult(float input[], float dz[], float output[], float original[],
        		int xDim, int yDim, int zDim, 
        		int maskSize, int maskOrigin, int clipZ, int slice) {
        	int length = xDim * yDim;
        	int offset = slice * length;
        	int x;
        	int y;
        	int mz;
        	int iz;
        	int i;
        	int index;
        	if ((clipZ != 0) && ((slice < (maskSize/2)) || (slice >= (zDim - (maskSize/2))))) {
        	    for (i = 0; i < length; i++) {
        	    	output[offset + i] = original[offset + i] * input[offset + i];
        	    }
        	    return;
        	} // if ((clipZ != 0) && ((slice < (maskSize/2)) || (slice >= (zDim - (maskSize/2)))))
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			float sumZ = 0.0f;
        			float dzVal = 0.0f;
        			for (mz = 0; mz < maskSize; mz++) {
        			    iz = slice - maskOrigin + mz ;
        			    i = iz * length + y * xDim + x;
        			    if ((iz >= 0) && (iz < zDim)) {
        			        dzVal = dz[mz];
        			        sumZ += input[i] * dzVal;
        			    }
        			} // for (my = 0; my < maskSize; my++)
        			index = offset + y * xDim + x;
        		    output[index] = original[index] * sumZ;
        		    if (maskSize == 0) {
        		    	output[index] = original[index] * input[index];
        		    }
        		} // for (x = 0; x < xDim; x++)
        	} // for (y = 0; y < yDim; y++)
        }
        
        /**
    	 * Creates the Convolution kernels (data arrays) in OpenCL.
    	 * Creates 3 1D arrays in OpenCL for the x-convolution kernel
    	 * the y-convolution kernel, and the z-convolution kernel.
    	 * @param index (0 for single-image deconvolution, 0,1 for dual-image)
    	 * @param sigmas
    	 */
    	private void initConvolutionBuffers(int index, float[] sigmas, float derivativeX[][], float derivativeY[][],
    			float derivativeZ[][], int kExtents[][], int kOrigins[][]) 
    	{

    		//
    		// Create the convolution buffers:    
    		//
    		float[] localSigmas = new float[sigmas.length];
    		if (useDeconvSigmaConversionFactor)
    		{
    			double conversion = 1.0 / (2*Math.sqrt(2*Math.log(2)));
    			for ( int i = 0; i < sigmas.length; i++ )
    			{
    				localSigmas[i] = (float) (sigmas[i] * conversion);
    			}
    		}
    		else
    		{
    			for ( int i = 0; i < sigmas.length; i++ )
    			{
    				localSigmas[i] = sigmas[i];
    			}
    		}
//    		for ( int i = 0; i < sigmas.length; i++ )
//    		{
//    			System.err.print( localSigmas[i] + " " );
//    		}
//    		System.err.println("");
//    		System.err.println("");
    		
    		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(localSigmas);
    		gkf.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
    		Kernel gaussianKernel = gkf.createKernel();
    		float[][] derivativeKernel = gaussianKernel.getData();

//    		for ( int i = 0; i < derivativeKernel[0].length; i++ )
//    			System.err.print( derivativeKernel[0][i] + " " );
//    		System.err.println("");
//    		System.err.println("");
//    		System.err.println("");

//    		for ( int i = 0; i < derivativeKernel[1].length; i++ )
//    			System.err.print( derivativeKernel[1][i] + " " );
//    		System.err.println("");
//    		System.err.println("");
//    		System.err.println("");

//    		for ( int i = 0; i < derivativeKernel[2].length; i++ )
//    			System.err.print( derivativeKernel[2][i] + " " );
//    		System.err.println("");
//    		System.err.println("");
//    		System.err.println("");

    		int[] kExtentsTemp = gaussianKernel.getExtents();
    		kExtents[index] = new int[kExtentsTemp.length];
    		for ( int i = 0; i < kExtents[index].length; i++ )
    		{
    			kExtents[index][i] = kExtentsTemp[i];
    		}

    		//	float[][] derivativeKernel = new float[3][];
    		//	derivativeKernel[0] = getKernel(localSigmas[0]);
    		//	derivativeKernel[1] = getKernel(localSigmas[1]);
    		//	derivativeKernel[2] = getKernel(localSigmas[2]);
    		//	int[] kExtents = new int[]{ derivativeKernel[0].length, derivativeKernel[1].length, derivativeKernel[2].length };

    		kOrigins[index] = new int[kExtents[index].length];
    		for ( int i = 0; i < kOrigins[index].length; i++ )
    		{
    			kOrigins[index][i] = (kExtents[index][i]-1)>>1;
    		}
//    		System.err.println( kExtents[index][0] + " " + kExtents[index][1] + " " + kExtents[index][2] );
//    		System.err.println( kOrigins[index][0] + " " + kOrigins[index][1] + " " + kOrigins[index][2] );
    		
    		// x-convolution buffer:
    		derivativeX[index] = new float[derivativeKernel[0].length];
    		for (int i = 0; i < derivativeKernel[0].length; i++) {
    			derivativeX[index][i] = derivativeKernel[0][i];
    		}
    		
    		// y-convolution buffer:
    		derivativeY[index] = new float[derivativeKernel[1].length];
    		for (int i = 0; i < derivativeKernel[1].length; i++) {
    			derivativeY[index][i] = derivativeKernel[1][i];
    		}
    		
    		// z-convolution buffer:
    		derivativeZ[index] = new float[derivativeKernel[2].length];
    		for (int i = 0; i < derivativeKernel[2].length; i++) {
    			derivativeZ[index][i] = derivativeKernel[2][i];
    		}
    	}

        /**
         * Discards extraneous slices from transform image.
         */
        /*
         * private void discardSlices(int middleSlice) { // TODO Auto-generated method stub
         * 
         * }
         */

        public ModelImage getSubGeoImage() {
            return subGeoImage;
        }

        public ModelImage getSubAriImage() {
            return subAriImage;
        }
    }

    private void threshold(final ModelImage baseImage, final double threshold) {
        for (int i = 0; i < baseImage.getDataSize(); i++) {
            if (baseImage.getDouble(i) <= thresholdIntensity) {
                baseImage.set(i, 0);
            }
        }
    }
}
