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
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.imageio.stream.FileImageOutputStream;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
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

public class PlugInAlgorithmGenerateFusion541b extends AlgorithmBase {

    
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
     */
    public PlugInAlgorithmGenerateFusion541b(ModelImage image1, boolean doSubsample, boolean doInterImages, boolean doGeoMean, boolean doAriMean, boolean doThreshold, 
                                                    double resX, double resY, double resZ, int concurrentNum, double thresholdIntensity, String mtxFileLoc, int middleSlice, File[] baseImageAr, File[] transformImageAr) {
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
        this.thresholdIntensity = thresholdIntensity;
        
        this.mtxFileLoc = mtxFileLoc;
        this.baseImageAr = baseImageAr;
        this.transformImageAr = transformImageAr;
        
        this.concurrentNum = concurrentNum;
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
    
    public class FusionAlg implements Callable, Runnable {

        private ModelImage baseImage, transformImage;
        
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
                threshold(transformImage);
            }
            
            if(doInterImages) {
                new ViewJFrameImage(baseImage);
            }
            
            rotate(AlgorithmRotate.Y_AXIS_MINUS);
            
            transform();
            
            discardSlices(middleSlice);
            
            if(doAriMean) {
                calcAriMean();
            }
            
            if(doGeoMean) {
                calcGeoMean();
            } 
            
            return true;
        }
        
        public ModelImage getResultImage() {
            
            return null;
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

        private void rotate(int mode) {
            AlgorithmRotate rotate = new AlgorithmRotate(transformImage, mode);
            rotate.run(); //transform image replaced
            transformImage = rotate.getDestImage();
            if(doInterImages) {
                ViewJFrameImage test = new ViewJFrameImage(transformImage);
            }
            
        }

        private void calcGeoMean() {
            // TODO Auto-generated method stub
            
        }

        private void calcAriMean() {
            // TODO Auto-generated method stub
            
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
    }

    
}