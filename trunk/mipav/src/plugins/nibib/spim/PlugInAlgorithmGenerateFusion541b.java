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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.imageio.stream.FileImageOutputStream;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath.Operator;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogImageMath;
import gov.nih.mipav.view.dialogs.JDialogScriptableTransform;

/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. 
 * By extending AlgorithmBase, it has no more functionality than any other algorithm in MIPAV.
 * No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmGenerateFusion541b extends AlgorithmBase {

    public enum SampleMode {
        DownsampleToBase("Downsample transformed image to base"),
        UpsampleToTransform("Upsample base image to transformed"),
        DownsampleUpsampleCombined("Do both sampling mechanisms");
        
        private String str;

        SampleMode(String str) {
            this.str = str;
        }

        public String toString() {
            return str;
        }
    }
    
    private ModelImage image;
    private boolean doAriMean;
    private boolean doSubsample;
    private boolean doInterImages;
    private boolean doGeoMean;
    private int middleSlice;
    private File[] baseImageAr;
    private File[] transformImageAr;
    private boolean doThreshold;
    private double resX;
    private double resY;
    private double resZ;
    private double thresholdIntensity;
    private String mtxFileLoc;
    private int concurrentNum;
    private Integer xMovement;
    private Integer yMovement;
    private Integer zMovement;
    private Collection<ModelImage> resultImageList;
    private SampleMode mode;
    private ArrayBlockingQueue<MeasureAlg> movementQueue;
    

    /**
     * Constructor.
     * @param doGeoMean 
     * @param doInterImages 
     * @param doSubsample 
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     * @param scale 
     * @param image2Intensity 
     * @param doSubsample 
     * @param middleSlice2 
     * @param thresholdIntensity 
     * @param resZ 
     * @param resY 
     * @param resX 
     * @param doThreshold 
     * @param mtxFileLoc 
     * @param zMovement 
     * @param yMovement 
     * @param xMovement 
     * @param mode 
     */
    public PlugInAlgorithmGenerateFusion541b(ModelImage image1, boolean doSubsample, boolean doInterImages, boolean doGeoMean, boolean doAriMean, boolean doThreshold, 
                                                    double resX, double resY, double resZ, int concurrentNum, double thresholdIntensity, String mtxFileLoc, int middleSlice, 
                                                    File[] baseImageAr, File[] transformImageAr, Integer xMovement, Integer yMovement, Integer zMovement, SampleMode mode) {
        super(null, image1);
        
        this.image = image1;
        this.doAriMean = doAriMean;
        this.doSubsample = doSubsample;
        this.doInterImages = doInterImages;
        this.doGeoMean = doGeoMean;
        this.doThreshold = doThreshold;
        
        this.middleSlice = middleSlice;
        this.resX = resX;
        this.resY = resY;
        this.resZ = resZ;
        this.xMovement = xMovement;
        this.yMovement = yMovement;
        this.zMovement = zMovement;
        this.thresholdIntensity = thresholdIntensity;
        
        this.mtxFileLoc = mtxFileLoc;
        this.baseImageAr = baseImageAr;
        this.transformImageAr = transformImageAr;
        
        this.concurrentNum = concurrentNum;
        
        this.mode = mode;
        
        this.resultImageList = Collections.synchronizedCollection(new ArrayList<ModelImage>());
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
        
        movementQueue = new ArrayBlockingQueue<MeasureAlg>(1);
        
        ExecutorService exec = Executors.newFixedThreadPool(concurrentNum);
        
        ArrayList<FusionAlg> algList = new ArrayList<FusionAlg>();
        for(int i=0; i<transformImageAr.length; i++) {
            FileIO io = new FileIO();
            
            ModelImage baseImage = io.readImage(baseImageAr[i].getAbsolutePath());
            ModelImage transformImage = io.readImage(transformImageAr[i].getAbsolutePath());
            
            FusionAlg algInstance = new FusionAlg(null, baseImage, transformImage);
            algList.add(algInstance);
            exec.execute(algInstance);
        }
        
        exec.shutdown();
        
        try {
            exec.awaitTermination(1, TimeUnit.DAYS);
        } catch (InterruptedException e) {
            MipavUtil.displayError("Program did not execute correctly");
            e.printStackTrace();
        }
        
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }
    
    public class MeasureAlg implements Runnable {

        private ModelImage baseImage, transformImage;
        
        private Frame parentFrame;

        private int xMeasure = 0, yMeasure = 0, zMeasure = 0;
        
        public MeasureAlg(Frame parentFrame, ModelImage baseImage, ModelImage transformImage) {
            this.parentFrame = parentFrame;
            this.baseImage = baseImage;
            this.transformImage = transformImage;
        }
        
        public void run() {
            long time = System.currentTimeMillis();
            
            AlgorithmImageCalculator imgCalc = new AlgorithmImageCalculator(baseImage, transformImage, AlgorithmImageCalculator.SUBTRACT, AlgorithmImageCalculator.CLIP, true, null);
            imgCalc.run();
            
            JDialogImageMath sumImg = new JDialogImageMath();
            sumImg.setOperator(Operator.SUM.getLegacyNum());
            sumImg.setClipMode(AlgorithmImageMath.CLIP);
            sumImg.setSeparateThread(false);
            sumImg.setImage(baseImage);
            sumImg.actionPerformed(new ActionEvent(parentFrame, 0, "Script"));
            
            ModelImage img = sumImg.getResultImage();
            ViewJFrameImage frame = new ViewJFrameImage(img);
            
            System.out.println((System.currentTimeMillis() - time));
            
            setMeasures();
        }
        
        private void setMeasures() {
            xMovement = xMeasure;
            yMovement = yMeasure;
            zMovement = zMeasure;
        }
    }

    public class FusionAlg implements Callable, Runnable {

        private ModelImage baseImage, transformImage;
        
        private ModelImage subGeoImage = null, subAriImage = null;
        
        private Frame parentFrame;
        
        public FusionAlg(Frame parentFrame, ModelImage baseImage, ModelImage transformImage) {
            this.parentFrame = parentFrame;
            this.baseImage = baseImage;
            this.transformImage = transformImage;
        }
        
        public void run() {
            call();
        }
        
        public Boolean call() {
            

            baseImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            transformImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            
            for(int i=0; i<baseImage.getFileInfo().length; i++) {
                baseImage.getFileInfo(i).setSliceThickness(0);
            }
            
            for(int i=0; i<transformImage.getFileInfo().length; i++) {
                transformImage.getFileInfo(i).setSliceThickness(0);
            }
            
            if(doThreshold) {
                threshold(baseImage);
            }
            
            if(doInterImages) {
                new ViewJFrameImage(baseImage);
            }
            
            rotate(AlgorithmRotate.Y_AXIS_MINUS);
            
            transform();
            
            switch (mode) {
            
            case DownsampleToBase:
                downsampleToBase();
                break;
            
            case DownsampleUpsampleCombined:
                downsampleUpsampleCombined();
                break;
                
            case UpsampleToTransform:
                upsampleToTransform();
                break;
            }
            
            if(doThreshold) {
                threshold(transformImage);
            }
            
            if(xMovement == null && yMovement == null && zMovement == null) {
                if(movementQueue.size() > 0) {
                    try {
                        movementQueue.take();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                } else {
                    MeasureAlg alg = new MeasureAlg(parentFrame, baseImage, transformImage);
                    movementQueue.add(alg);
                    alg.run();
                }
            }
            
            //discardSlices(middleSlice);
            
            if(doAriMean) {
                calcAriMean();
                
                resultImageList.add(subAriImage);
            }
            
            if(doGeoMean) {
                calcGeoMean();
                
                resultImageList.add(subGeoImage);
            } 
            
            return true;
        }
        
        

        private void transform() {
            JDialogScriptableTransform transform = new JDialogScriptableTransform(parentFrame, transformImage);
            transform.setPadFlag(true);
            transform.setMatrix(transform.readTransformMatrixFile(mtxFileLoc));
            transform.setImage25D(false);
            transform.setSeparateThread(false);
            transform.setClipFlag(true);
            transform.setDimAndResXYZ();
            transform.setUnits(transformImage.getUnitsOfMeasure());
            transform.setOutDimensions(new int[]{118,430,312});//transformImage.getExtents());
            transform.setOutResolutions(transformImage.getResolutions(0));
            transform.actionPerformed(new ActionEvent(this, 0, "Script"));
            transformImage = transform.getResultImage();
            if(doInterImages) {
                new ViewJFrameImage(transformImage);
            }
        }
        
        private void downsampleUpsampleCombined() {
            TransMatrix mat = new TransMatrix(4);
            mat.MakeIdentity();
            mat.set(0, 0, (2*transformImage.getResolutions(0)[0]) / baseImage.getResolutions(0)[0]);
            mat.set(2, 2, (2*transformImage.getResolutions(0)[2]) / baseImage.getResolutions(0)[2]);
            
            transformImage = subTransform(transformImage, mat);
            
            float zRes = transformImage.getResolutions(0)[2]/2;
            transformImage.setResolutions(new float[]{baseImage.getResolutions(0)[0], baseImage.getResolutions(0)[1], zRes});
            
            mat = new TransMatrix(4);
            mat.MakeIdentity();
            mat.set(0, 0, (2*baseImage.getResolutions(0)[0]) / transformImage.getResolutions(0)[0]);
            mat.set(2, 2, (2*baseImage.getResolutions(0)[2]) / transformImage.getResolutions(0)[2]);
            
            baseImage = subTransform(baseImage, mat);
            
            zRes = transformImage.getResolutions(0)[2]/2;
            baseImage.setResolutions(new float[]{transformImage.getResolutions(0)[0], transformImage.getResolutions(0)[1], zRes});
            
            
        }
        
        private void downsampleToBase() {
            TransMatrix mat = new TransMatrix(4);
            mat.MakeIdentity();
            mat.set(0, 0, transformImage.getResolutions(0)[0] / baseImage.getResolutions(0)[0]);
            mat.set(2, 2, transformImage.getResolutions(0)[2] / baseImage.getResolutions(0)[2]);
            
            transformImage = subTransform(transformImage, mat);
            
            float zRes = transformImage.getResolutions(0)[2];
            transformImage.setResolutions(new float[]{baseImage.getResolutions(0)[0], baseImage.getResolutions(0)[1], zRes});
            if(doInterImages) {
                new ViewJFrameImage(transformImage);
            }
        }
        
        private void upsampleToTransform() {
            TransMatrix mat = new TransMatrix(4);
            mat.MakeIdentity();
            mat.set(0, 0, baseImage.getResolutions(0)[0] / transformImage.getResolutions(0)[0]);
            mat.set(2, 2, baseImage.getResolutions(0)[2] / transformImage.getResolutions(0)[2]);
            
            baseImage = subTransform(baseImage, mat);
            
            float zRes = transformImage.getResolutions(0)[2];
            baseImage.setResolutions(new float[]{transformImage.getResolutions(0)[0], transformImage.getResolutions(0)[1], zRes});
            if(doInterImages) {
                new ViewJFrameImage(baseImage);
            }
        }
        
        private ModelImage subTransform(ModelImage image, TransMatrix mat) {
            JDialogScriptableTransform transform = new JDialogScriptableTransform(parentFrame, image);
            transform.setPadFlag(true);
            transform.setMatrix(mat);
            transform.setImage25D(false);
            transform.setSeparateThread(false);
            transform.setClipFlag(true);
            transform.setDimAndResXYZ();
            transform.setUnits(image.getUnitsOfMeasure());
            transform.setOutDimensions(image.getExtents());//transformImage.getExtents());
            transform.setOutResolutions(image.getResolutions(0));
            transform.actionPerformed(new ActionEvent(this, 0, "Script"));
            
            return transform.getResultImage();
        }

        private void rotate(int mode) {
            AlgorithmRotate rotate = new AlgorithmRotate(transformImage, mode);
            rotate.run(); //transform image replaced
            transformImage = rotate.getDestImage();
            if(doInterImages) {
                ViewJFrameImage test = new ViewJFrameImage(transformImage);
            }
            
        }

        private void calcGeoMean() {
            subGeoImage = ViewUserInterface.getReference().createBlankImage(baseImage.getFileInfo(0));
            int transformX, transformY, transformZ;
            //new ViewJFrameImage(transformImage);
            for(int i=0; i<baseImage.getExtents()[0]; i++) {
                transformX = i-xMovement;
                for(int j=0; j<baseImage.getExtents()[1]; j++) {
                    transformY = j-yMovement;
                    for(int k=0; k<baseImage.getExtents()[2]; k++) {
                        transformZ = k-zMovement;
                        if(transformX >= 0 && transformX < transformImage.getExtents()[0] && 
                                transformY >= 0 && transformY < transformImage.getExtents()[1] && 
                                transformZ >= 0 && transformZ < transformImage.getExtents()[2]) {
                            subGeoImage.set(i, j, k, Math.sqrt(baseImage.getDouble(i, j, k)*transformImage.getDouble(transformX, transformY, transformZ)));
                        } else {
                            subGeoImage.set(i, j, k, baseImage.getDouble(i, j, k));
                        }
                    }
                }
            }
            
            subGeoImage.calcMinMax();
            
            subGeoImage.setImageName(baseImage.getImageName()+"_GeoMeanFused");
            
            if(doInterImages) {
                new ViewJFrameImage(subGeoImage);
            }
        }

        private void calcAriMean() {
            subAriImage = ViewUserInterface.getReference().createBlankImage(baseImage.getFileInfo(0));
            int transformX, transformY, transformZ;
            //new ViewJFrameImage(transformImage);
            for(int i=0; i<baseImage.getExtents()[0]; i++) {
                transformX = i-xMovement;
                for(int j=0; j<baseImage.getExtents()[1]; j++) {
                    transformY = j-yMovement;
                    for(int k=0; k<baseImage.getExtents()[2]; k++) {
                        transformZ = k-zMovement;
                        if(transformX >= 0 && transformX < transformImage.getExtents()[0] && 
                                transformY >= 0 && transformY < transformImage.getExtents()[1] && 
                                transformZ >= 0 && transformZ < transformImage.getExtents()[2]) {
                            subAriImage.set(i, j, k, (baseImage.getDouble(i, j, k) + transformImage.getDouble(transformX, transformY, transformZ))/2.0 );
                        } else {
                            subAriImage.set(i, j, k, baseImage.getDouble(i, j, k));
                        }
                    }
                }
            }
            
            subAriImage.calcMinMax();
            
            subAriImage.setImageName(baseImage.getImageName()+"_AriMeanFused");
            
            if(doInterImages) {
                new ViewJFrameImage(subAriImage);
            }
        }

        /**
         * Discards extraneous slices from transform image.
         */
        private void discardSlices(int middleSlice) {
            // TODO Auto-generated method stub
            
        }

        private void threshold(ModelImage baseImage) {
            for(int i=0; i<baseImage.getDataSize(); i++) {
                if(baseImage.getDouble(i) <= thresholdIntensity) {
                    baseImage.set(i, 0);
                }
            }
        }

        public ModelImage getSubGeoImage() {
            return subGeoImage;
        }

        public ModelImage getSubAriImage() {
            return subAriImage;
        }
    }

    
}