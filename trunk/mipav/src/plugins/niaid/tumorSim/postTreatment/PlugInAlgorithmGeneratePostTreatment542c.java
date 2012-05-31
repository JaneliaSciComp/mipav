package niaid.tumorSim.postTreatment;
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

import niaid.tumorSim.createMap.PlugInAlgorithmCreateTumorMap542c;

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

public class PlugInAlgorithmGeneratePostTreatment542c extends AlgorithmBase {

	/** Whether to perform a gaussian blur */
    private double image1Intensity, image2Intensity;

    private ModelImage image1a, image2a;
    
    private ModelImage image1b, image2b;
    
    private ModelImage image1c, image2c;

    private double image1Scale, image2Scale;

    private double image1Noise, image2Noise;

    private ModelImage postTreatment;

    private double stdDevNum;

    private double image1ThresholdLower, image1ThresholdUpper;

    private double image2ThresholdLower, image2ThresholdUpper;

    private double postThresholdLower, postThresholdUpper;

    private boolean image1cVOI, image2cVOI, postVOI;

    private double normalTissue;

    private double image1IntensityStd, image2IntensityStd, normalTissueStd;

    private ModelImage image1aTumor;

    private ModelImage image2aTumor;
    
    /**
     * Constructor.
     * @param image1Tumor 
     * @param image1Noise 
     * @param image1Scale 
     * @param image1Intensity 
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     * @param scale 
     * @param image2Intensity 
     * @param image1Intensity 
     * @param image1TresholdUpper 
     * @param image1ThresholdLower 
     * @param image1cVOI 
     * @param image2Tumor 
     * @param stdDevNum 
     * @param stdDevNum2 
     * @param image2ThresholdUpper 
     * @param image2cVOI 
     * @param postThresholdUpper 
     * @param postThresholdLower 
     * @param postVOI 
     * @param normalTissue 
     */
    public PlugInAlgorithmGeneratePostTreatment542c(ModelImage image1, ModelImage image1aTumor, double image1Intensity, double image1IntensityStd, double image1Scale, double image1Noise, 
                                                    double image1ThresholdLower, double image1ThresholdUpper, 
                                                    boolean image1cVOI, ModelImage image2, ModelImage image2aTumor, double image2Intensity, double image2IntensityStd, double image2Scale, double image2Noise, 
                                                    double image2ThresholdLower, double image2ThresholdUpper, 
                                                    boolean image2cVOI, double stdDevNum, double postThresholdLower, double postThresholdUpper, boolean postVOI, double normalTissue, double normalTissueStd) {
        super(null, image1);
        
        this.image1a = image1;
        this.image2a = image2;
        
        this.image1aTumor = image1aTumor;
        this.image2aTumor = image2aTumor;
        
        this.image1b = (ModelImage) image1.clone();
        image1b.setImageName("image1b");
        
        this.image1Intensity = image1Intensity;
        this.image1IntensityStd = image1IntensityStd;
        this.image1Scale = image1Scale;
        this.image1Noise = image1Noise;
        this.image1ThresholdLower = image1ThresholdLower;
        this.image1ThresholdUpper = image1ThresholdUpper;
        this.image1cVOI = image1cVOI;
        
        this.image2b = (ModelImage) image2.clone();
        image2b.setImageName("image2b");
        
        this.image2Intensity = image2Intensity;
        this.image2IntensityStd = image2IntensityStd;
        this.image2Scale = image2Scale;
        this.image2Noise = image2Noise;
        this.image2ThresholdLower = image2ThresholdLower;
        this.image2ThresholdUpper = image2ThresholdUpper;
        this.image2cVOI = image2cVOI;
        
        this.postThresholdLower = postThresholdLower;
        this.postThresholdUpper = postThresholdUpper;
        this.postVOI = postVOI;
        
        this.normalTissue = normalTissue;
        this.normalTissueStd = normalTissueStd;
        
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
        //b image is subtractive scale factor (so image1Scale=0 would produce blank image1b (and image1c = image1a)
        scaleAndRemoveTumor(image1b, image1aTumor, image1Intensity, 1-image1Scale); 
        scaleAndRemoveTumor(image2b, image2aTumor, image2Intensity, 1-image2Scale);
        
        
        image1c = (ModelImage) image1b.clone();
        image1c.setImageName("image1c");
        image1c = subtractImages(image1c, image1a, image1b);
        
        threshold(image1c, image1ThresholdLower, image1ThresholdUpper);
        
        if(image1cVOI) {
            PlugInAlgorithmCreateTumorMap542c.createVOI(image1c, image1Intensity-image1Intensity*stdDevNum*image1IntensityStd, image1Intensity+image1Intensity*stdDevNum*image1IntensityStd);
        }
        
        image2c = (ModelImage) image2b.clone();
        image2c.setImageName("image2c");
        image2c = subtractImages(image2c, image2a, image2b);
        
        if(image2cVOI) {
            PlugInAlgorithmCreateTumorMap542c.createVOI(image2c, image2Intensity-image2Intensity*stdDevNum*image2IntensityStd, image2Intensity+image2Intensity*stdDevNum*image2IntensityStd);
        }
        
        threshold(image2c, image2ThresholdLower, image2ThresholdUpper);
        
        postTreatment = (ModelImage) image2c.clone();
        postTreatment.setImageName("postTreatment");
        postTreatment = subtractImages(postTreatment, image2c, image1c);
        
        threshold(postTreatment, postThresholdLower, postThresholdUpper);
        
        if(postVOI) {
            //calculate propagation of error
            double postStdDev = Math.sqrt(image1IntensityStd*image1IntensityStd + image2IntensityStd*image2IntensityStd);
            double postIntensity = image2Intensity - image1Intensity;
            PlugInAlgorithmCreateTumorMap542c.createVOI(postTreatment, postIntensity-postIntensity*stdDevNum*postStdDev, postIntensity+postIntensity*stdDevNum*postStdDev);
        }
        
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
            
            //printData("Slice "+z, numPixelsSlice, sumIntensitiesSlice, numNegPixelsSlice, sumNegIntensitiesSlice, numPosPixelsSlice, sumPosIntensitiesSlice);
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

    /**
     * Scales all partial voluming pixels by scaling factor, removes all other pixels
     * 
     * @param image
     * @param tumorImage
     * @param tumorIntensity
     * @param partialVolumeScale
     */
    private void scaleAndRemoveTumor(ModelImage image, ModelImage tumorImage, double tumorIntensity, double partialVolumeScale) {
        double intensity = 0.0;
        for(int i=0; i<image.getDataSize(); i++) {
            intensity = tumorImage.getDouble(i);
            if(intensity != 0) {
                if(intensity == tumorIntensity) {
                    image.set(i, 0);
                } else {
                    image.set(i, intensity*partialVolumeScale);
                }
            } else {
                image.set(i, 0);
            }
        }
    }
    
    private void threshold(ModelImage image, double lowerBound, double upperBound) {
        double intensity = 0.0;
        for(int i=0; i<image.getDataSize(); i++) {
            intensity = image.getDouble(i);
            if(intensity >= lowerBound && intensity <= upperBound) {
                image.set(i, 0);
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