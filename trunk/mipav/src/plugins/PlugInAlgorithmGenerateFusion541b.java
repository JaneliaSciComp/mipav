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

import java.awt.event.ActionEvent;
import java.io.File;

import javax.imageio.stream.FileImageOutputStream;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
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
                                                    double resX, double resY, double resZ, int middleSlice, double thresholdIntensity, String mtxFileLoc, int middleSlice2, File[] baseImageAr, File[] transformImageAr) {
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
        FileIO io = new FileIO();
        for(int i=0; i<transformImageAr.length; i++) {
            
            ModelImage baseImage = io.readImage(baseImageAr[i].getAbsolutePath());
            ModelImage transformImage = io.readImage(transformImageAr[i].getAbsolutePath());
            
            baseImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            transformImage.setResolutions(new float[]{(float) resX, (float) resY, (float) resZ});
            
            if(doThreshold) {
                threshold(baseImage);
                threshold(transformImage);
            }
            
            AlgorithmRotate rotate = new AlgorithmRotate(transformImage, AlgorithmRotate.Y_AXIS_MINUS);
            rotate.run(); //transform image should hav been replaced
            
            JDialogScriptableTransform transform = new JDialogScriptableTransform();
            transform.setPadFlag(true);
            transform.readTransformMatrixFile(mtxFileLoc);
            transform.setImage25D(false);
            transform.setSeparateThread(false);
            transform.actionPerformed(new ActionEvent(this, 0, "OK"));
            transformImage = transform.getResultImage();
            if(doInterImages) {
                new ViewJFrameImage(transformImage);
            }
            
            discardSlices(baseImage, transformImage, middleSlice);
            
            if(doAriMean) {
                calcAriMean(baseImage, transformImage);
            }
            
            if(doGeoMean) {
                calcGeoMean(baseImage, transformImage);
            }
        }

    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    private void calcGeoMean(ModelImage baseImage, ModelImage transformImage) {
        // TODO Auto-generated method stub
        
    }

    private void calcAriMean(ModelImage baseImage, ModelImage transformImage) {
        // TODO Auto-generated method stub
        
    }

    private void discardSlices(ModelImage baseImage, ModelImage transformImage,
            int middleSlice2) {
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