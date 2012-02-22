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

import java.io.File;

import javax.imageio.stream.FileImageOutputStream;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. 
 * By extending AlgorithmBase, it has no more functionality than any other algorithm in MIPAV.
 * No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmGenerateFusion541a extends AlgorithmBase {

    
    private ModelImage image;
    private boolean doAriMean;
    private boolean doSubsample;
    private boolean doInterImages;
    private boolean doGeoMean;
    private int middleSlice;
    private File[] baseImageAr;
    private File[] transformImageAr;

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
     */
    public PlugInAlgorithmGenerateFusion541a(ModelImage image1, boolean doSubsample, boolean doInterImages, boolean doGeoMean, 
                                                    boolean doAriMean, int middleSlice, File[] baseImageAr, File[] transformImageAr) {
        super(null, image1);
        
        this.image = image1;
        this.doAriMean = doAriMean;
        this.doSubsample = doSubsample;
        this.doInterImages = doInterImages;
        this.doGeoMean = doGeoMean;
        
        this.middleSlice = middleSlice;
        
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
            
        }

    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    private void reportStatistics(ModelImage postTreatment2) {
        double sumIntensitiesSlice = 0, sumPosIntensitiesSlice = 0, sumNegIntensitiesSlice = 0;
        double sumIntensitiesTotal = 0, sumPosIntensitiesTotal = 0, sumNegIntensitiesTotal = 0;
        double intensity = 0;
        int numPixelsSlice = 0, numNegPixelsSlice = 0, numPosPixelsSlice = 0, 
                numPixelsTotal = 0, numPosPixelsTotal = 0, numNegPixelsTotal = 0;
        Preferences.data("Slice number\tTotal pixels\tTotal Avg\tNeg Pixels\tNeg Average\tPos Pixels\tPos Average\n");       
        int z = 0;
        for(int i=0; i<1; i++) {
            if(i != 0 && i %1 == 0) {
                if(numPixelsTotal > 0) {
                    sumIntensitiesTotal += sumIntensitiesSlice;
                    sumPosIntensitiesTotal += sumPosIntensitiesSlice;
                    sumNegIntensitiesTotal += sumNegIntensitiesSlice;
                    
                    numPixelsTotal += numPixelsSlice;
                    numPosPixelsTotal += numPosPixelsSlice;
                    numNegPixelsTotal += numNegPixelsSlice;
                    
                    printData("Slice "+z, numPixelsSlice, sumIntensitiesSlice, numNegPixelsSlice, sumNegIntensitiesSlice, numPosPixelsSlice, sumPosIntensitiesSlice);
                }
                sumIntensitiesSlice = 0;
                sumPosIntensitiesSlice = 0;
                sumNegIntensitiesSlice = 0;
                
                numPixelsSlice = 0;
                numPosPixelsSlice = 0;
                numNegPixelsSlice = 0;
                
                z++;
            }
            intensity = 1;
            if(intensity != 0) {
                if(intensity > 0) {
                    sumPosIntensitiesSlice += intensity;
                    numPosPixelsSlice++;
                } else { //intensity < 0
                    sumNegIntensitiesSlice += intensity;
                    numNegPixelsSlice++;
                }
                sumIntensitiesSlice += intensity;
                numPixelsSlice++;
            }
        }
        
        if(numPixelsTotal > 0) {
            sumIntensitiesTotal += sumIntensitiesSlice;
            sumPosIntensitiesTotal += sumPosIntensitiesSlice;
            sumNegIntensitiesTotal += sumNegIntensitiesSlice;
            
            numPixelsTotal += numPixelsSlice;
            numPosPixelsTotal += numPosPixelsSlice;
            numNegPixelsTotal += numNegPixelsSlice;
            
            printData("Slice "+z, numPixelsSlice, sumIntensitiesSlice, numNegPixelsSlice, sumNegIntensitiesSlice, numPosPixelsSlice, sumPosIntensitiesSlice);
        }
        
        printData("All slices", numPixelsTotal, sumIntensitiesTotal, numNegPixelsTotal, sumNegIntensitiesTotal, numPosPixelsTotal, sumPosIntensitiesTotal);
    }

    private void printData(String string, int numPixels, double sumIntensities, 
                                            int numNegPixels, double sumNegIntensities, 
                                            int numPosPixels, double sumPosIntensities) {
        Preferences.data(string+"\t"+numPixels+"\t"+(sumIntensities/numPixels)+"\t"+
                                            numNegPixels+"\t"+(sumNegIntensities/numNegPixels)+"\t"+
                                            numPosPixels+"\t"+(sumPosIntensities/numPosPixels));
    }

    private ModelImage subtractImages(ModelImage destImage, ModelImage imagea, ModelImage imageb) {
        AlgorithmImageCalculator algo1 = new AlgorithmImageCalculator(destImage, imagea, imageb, AlgorithmImageCalculator.SUBTRACT, AlgorithmImageCalculator.PROMOTE, true, null);
        algo1.setRunningInSeparateThread(false);
        algo1.run();
        //destImage.getParentFrame().setVisible(false);
        
        return algo1.getDestImage();
    }

    private void scaleAndRemoveTumor(ModelImage image,
            double imageIntensity, Double imageScale, Double imageNoise) {
        double lowerBound = imageIntensity - imageNoise;
        double upperBound = imageIntensity + imageNoise;
        double intensity = 0;
        for(int i=0; i<image.getDataSize(); i++) {
            intensity = image.getDouble(i);
            if(intensity != 0) {
                if(intensity >= lowerBound && intensity <= upperBound) {
                    image.set(i, 0);
                } else {
                    image.set(i, intensity*imageScale);
                }
            }
        }
    }
}