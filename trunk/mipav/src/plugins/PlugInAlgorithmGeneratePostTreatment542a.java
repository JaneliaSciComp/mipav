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

import java.util.Random;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
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

public class PlugInAlgorithmGeneratePostTreatment542a extends AlgorithmBase {

	/** Whether to perform a gaussian blur */
    private double image1Intensity, image2Intensity;

    private ModelImage image1a, image2a;
    
    private ModelImage image1b, image2b;
    
    private ModelImage image1c, image2c;

    private double image1Scale, image2Scale;

    private double image1Noise, image2Noise;

    private ModelImage postTreatment;

    private double stdDevNum;
    
    /**
     * Constructor.
     * @param image1Noise 
     * @param image1Scale 
     * @param image1Intensity 
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     * @param scale 
     * @param image2Intensity 
     * @param image1Intensity 
     * @param stdDevNum 
     */
    public PlugInAlgorithmGeneratePostTreatment542a(ModelImage image1, double image1Intensity, double image1Scale, double image1Noise, 
                                                    ModelImage image2, double image2Intensity, double image2Scale, double image2Noise, double stdDevNum) {
        super(null, image1);
        
        this.image1a = image1;
        this.image2a = image2;
        
        this.image1b = (ModelImage) image1.clone();
        image1b.setImageName("image1b");
        
        this.image1Intensity = image1Intensity;
        this.image1Scale = image1Scale;
        this.image1Noise = image1Noise;
        
        this.image2b = (ModelImage) image2.clone();
        image2b.setImageName("image2b");
        
        this.image2Intensity = image2Intensity;
        this.image2Scale = image2Scale;
        this.image2Noise = image2Noise;
        
        this.stdDevNum = stdDevNum;
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
        scaleAndRemoveTumor(image1b, image1Intensity, image1Scale, image1Noise);
        scaleAndRemoveTumor(image2b, image2Intensity, image2Scale, image2Noise);
        
        
        image1c = (ModelImage) image1b.clone();
        image1c.setImageName("image1c");
        image1c = subtractImages(image1c, image1a, image1b);
        
        image2c = (ModelImage) image2b.clone();
        image2c.setImageName("image2c");
        image2c = subtractImages(image2c, image2a, image2b);
        
        postTreatment = (ModelImage) image2c.clone();
        postTreatment.setImageName("postTreatment");
        postTreatment = subtractImages(postTreatment, image2c, image1c);
        
        reportStatistics(postTreatment, image1Intensity > image2Intensity ? image1Intensity : image2Intensity, 
                                            image1Noise > image2Noise ? image1Noise : image2Noise);

    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    private void reportStatistics(ModelImage postTreatment2, double maxIntensity, double imageMaxNoise) {
        double sumIntensitiesSlice = 0, sumPosIntensitiesSlice = 0, sumNegIntensitiesSlice = 0;
        double sumIntensitiesTotal = 0, sumPosIntensitiesTotal = 0, sumNegIntensitiesTotal = 0;
        double intensity = 0;
        int numPixelsSlice = 0, numNegPixelsSlice = 0, numPosPixelsSlice = 0, 
                numPixelsTotal = 0, numPosPixelsTotal = 0, numNegPixelsTotal = 0;
        Preferences.data("Slice number\tTotal pixels\tTotal Avg\tNeg Pixels\tNeg Average\tPos Pixels\tPos Average\n");       
        int z = 0;
        for(int i=0; i<postTreatment.getDataSize(); i++) {
            if(i != 0 && i % postTreatment.getSliceSize() == 0) {
                if(numPixelsSlice > 0) {
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
            intensity = postTreatment.get(i).doubleValue();
            if(intensity < -maxIntensity*stdDevNum*imageMaxNoise || intensity > maxIntensity*stdDevNum*imageMaxNoise) {
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
        
        if(numPixelsSlice > 0) {
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
        double totalAverage = (sumIntensities/numPixels);
        double negAverage = (sumNegIntensities/numNegPixels);
        double posAverage = (sumPosIntensities/numPosPixels);
        
        Preferences.data(string+"\t"+numPixels+"\t"+totalAverage+"\t"+
                                            numNegPixels+"\t"+negAverage+"\t"+
                                            numPosPixels+"\t"+posAverage+"\n");
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
        double lowerBound = imageIntensity - 2*imageNoise;
        double upperBound = imageIntensity + 2*imageNoise;
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

    public ModelImage getImage1b() {
        return image1b;
    }

    public ModelImage getImage2b() {
        return image2b;
    }

    public ModelImage getImage1c() {
        return image1c;
    }

    public ModelImage getImage2c() {
        return image2c;
    }

    public ModelImage getPostTreatment() {
        return postTreatment;
    }
    
    
}