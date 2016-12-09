package mtry;
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


import java.io.IOException;


import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.ImageOrientation;
import gov.nih.mipav.model.file.FileInfoBase.Modality;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


/**
 * This class recalculates mo and dceFullTre to provide better estimates using the inverse Ernst equation
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmMTry534d extends AlgorithmBase {

    /** Selected images with varying inverstion times. */
    private ModelImage minImage, medImage, maxImage;

    /** Max and min t1 values to perform fitting, defaults are 100, 7000 */
    private double t1Min, t1Max;

    /** Requested precision of fit*/
    private double precision;

    /** Inversion times used for each scan (usually detected in image information.) */
    private double invTimeMin, invTimeMed, invTimeMax;

    /** T1 maxinmum and minimum values used for function fitting, from t1Min and t1Max */
    private double funcMin, funcMax;

    /** Whether to perform reconstruction of image */
    private boolean doReconstruct;

    /** How many channels were used by scanner to acquire images */
    private int numChannel;

    /** Dimensions for output images. */
    private int xDim, yDim, zDim;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param numChannel 
     * @param  srcImg       Source image model, requires 3D images.
     */
    public PlugInAlgorithmMTry534d(ModelImage resultImage, ModelImage minImage, ModelImage medImage, ModelImage maxImage, 
            double t1Min, double t1Max, double precision, double invTimeMin, double invTimeMed, double invTimeMax, 
            boolean doReconstruct, int numChannel) {
        super(resultImage, minImage);
        this.minImage = minImage;
        this.medImage = medImage;
        this.maxImage = maxImage;
        this.t1Min = t1Min;
        this.t1Max = t1Max;
        this.precision = precision;
        this.invTimeMin = invTimeMin;
        this.invTimeMed = invTimeMed;
        this.invTimeMax = invTimeMax;
        this.doReconstruct = doReconstruct;
        this.numChannel = numChannel;

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
    	if(srcImage.getNDims() < 3) {
    		calc2D();
        } else {
    		calc3D();
        }
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    private void calc2D() {
    	fireProgressStateChanged("Message 2D: "+srcImage.getImageName());
     
    	MipavUtil.displayError("This algorithm is not designed for 2D images, since it requires complex image components.");
    }
    
    private void calc3D() {
    	
        xDim = minImage.getExtents()[0];
        yDim = minImage.getExtents()[1];
        zDim = minImage.getExtents()[2] / (2*numChannel);
        
    	int size = (int)((t1Max - t1Min)/precision);
    	double[] t1Val = new double[size];
    	double newX = t1Min;
    	for(int i=0; i<t1Val.length; i++) {
    	    t1Val[i] = t1Func(newX);
    	    newX += precision;
    	}
    	System.out.println("New x is "+newX+" while t1Max is "+t1Max);
    	
    	funcMin = t1Val[0];
    	funcMax = t1Val[0];
    	for(int i=0; i<t1Val.length; i++) {
    	    if(t1Val[i] < funcMin) {
    	        funcMin = t1Val[i];
    	    }
    	    if(t1Val[i] > funcMax) {
                funcMax = t1Val[i];
            }
    	}
    	
    	System.out.println("Function range is from "+funcMin+" to "+funcMax);
    	
    	double[] t1ValDiff = new double[t1Val.length-1];
    	for(int i=0; i<t1ValDiff.length; i++) {
    	    t1ValDiff[i] = t1Val[i+1] - t1Val[i];
    	}
    	
    	double t1Precision = precision;
    	for(int i=0; i<t1ValDiff.length; i++) {
    	    if(t1ValDiff[i] < t1Precision) {
    	        t1Precision = t1ValDiff[i];
    	    }
    	}
    	
    	//System.out.println("New precision value is: "+t1Precision);
    	
    	t1Precision = Math.floor(t1Precision*(Math.pow(10, Math.ceil(-Math.log10(t1Precision/10)))))*Math.pow(10, Math.ceil(Math.log10(t1Precision/100)));
    	System.out.println("Numerical errors correction result: "+t1Precision);
    	
    	funcMin = Math.ceil(funcMin/t1Precision)*t1Precision;
    	funcMax = Math.floor(funcMax/t1Precision)*t1Precision;
    	
    	System.out.println("New function range is from "+funcMin+" to "+funcMax+" and size "+((funcMax - funcMin) / t1Precision));
    	
    	int t1TableSize = (int)((funcMax - funcMin) / t1Precision);
    	System.out.println("T1 table size: "+t1TableSize);

    	double[] t1Table = new double[t1TableSize];
    	double funcValue = funcMin;
    	int t1ValIndex = 0;
    	double diff = 0;
    	for(int i=0; i<t1TableSize; i++) {
    	    diff = Math.abs(funcValue - t1Val[t1ValIndex]);
findClosest:while(t1ValIndex+1 < t1Val.length) {
    	        if(diff > Math.abs(funcValue - t1Val[t1ValIndex+1])) {
    	            t1ValIndex++;
    	        } else {
    	            break findClosest;
    	        }
    	    }
    	    t1Table[i] = t1ValIndex*precision + t1Min;
    	    funcValue += t1Precision;
    	}
    	
    	//Reshape the images as three complex images, and an additional phase image
    	double[][] minImageComplexReal = new double[xDim*yDim*zDim][numChannel], minImageComplexImag = new double[xDim*yDim*zDim][numChannel], 
    	            medImageComplexReal = new double[xDim*yDim*zDim][numChannel], medImageComplexImag = new double[xDim*yDim*zDim][numChannel],
    	            maxImageComplexReal = new double[xDim*yDim*zDim][numChannel], maxImageComplexImag = new double[xDim*yDim*zDim][numChannel], 
    	            negPhaseReal = new double[xDim*yDim*zDim][numChannel], negPhaseImag = new double[xDim*yDim*zDim][numChannel];
    	
    	rescaleToComplex(minImage, minImageComplexReal, minImageComplexImag);
    	rescaleToComplex(medImage, medImageComplexReal, medImageComplexImag);
    	rescaleToComplex(maxImage, maxImageComplexReal, maxImageComplexImag);
    	rescaleToComplex(maxImage, negPhaseReal, negPhaseImag);
    	
    	if(doReconstruct) {
        	ModelImage minImageRecon = reconstruct(minImageComplexReal, minImageComplexImag, "minImageRecon"+invTimeMin);
        	ModelImage medImageRecon = reconstruct(medImageComplexReal, medImageComplexImag, "medImageRecon"+invTimeMed);
        	ModelImage maxImageRecon = reconstruct(maxImageComplexReal, maxImageComplexImag, "maxImageRecon"+invTimeMax);
        	
        	ViewJFrameImage minRecon = new ViewJFrameImage(minImageRecon);
        	minRecon.setVisible(true);
        	
        	ViewJFrameImage medRecon = new ViewJFrameImage(medImageRecon);
        	medRecon.setVisible(true);
            
            ViewJFrameImage maxRecon = new ViewJFrameImage(maxImageRecon);
            maxRecon.setVisible(true);
    	}
    	
        double pow, mag;
        for(int i=0; i<maxImageComplexReal.length; i++) {
            for(int j=0; j<maxImageComplexReal[i].length; j++) {
                pow = Math.pow(maxImageComplexReal[i][j], 2.0) + Math.pow(maxImageComplexImag[i][j], 2.0);
                mag = Math.sqrt(pow);
                negPhaseReal[i][j] = (mag*maxImageComplexReal[i][j])/pow;
                negPhaseImag[i][j] = (-mag*maxImageComplexImag[i][j])/pow;
            }
        }
        
        double[] t1Real = new double[xDim*yDim*zDim];
        double[][] tempNumReal = new double[xDim*yDim*zDim][numChannel];
        double[][] tempNumImag = new double[xDim*yDim*zDim][numChannel];
        double[][] tempDenReal = new double[xDim*yDim*zDim][numChannel];
        double[][] tempDenImag = new double[xDim*yDim*zDim][numChannel];
        
        double realFactor;
        
        for(int i=0; i<t1Real.length; i++) {
            for(int j=0; j<tempNumReal[i].length; j++) {
                tempNumReal[i][j] = (maxImageComplexReal[i][j] - medImageComplexReal[i][j]);
                tempNumImag[i][j] = (maxImageComplexImag[i][j] - medImageComplexImag[i][j]);
                             
                realFactor = tempNumReal[i][j]*negPhaseReal[i][j] - tempNumImag[i][j]*negPhaseImag[i][j];
                tempNumImag[i][j] = tempNumImag[i][j]*negPhaseReal[i][j] + tempNumReal[i][j]*negPhaseImag[i][j];
                
                tempNumReal[i][j] = realFactor;
                
                tempDenReal[i][j] = (medImageComplexReal[i][j] - minImageComplexReal[i][j]);
                tempDenImag[i][j] = (medImageComplexImag[i][j] - minImageComplexImag[i][j]);
                
                realFactor = tempDenReal[i][j]*negPhaseReal[i][j] - tempDenImag[i][j]*negPhaseImag[i][j];
                tempDenImag[i][j] = tempDenImag[i][j]*negPhaseReal[i][j] + tempDenReal[i][j]*negPhaseImag[i][j];
                
                tempDenReal[i][j] = realFactor;
            }
        }

        double[] avgNumReal = getMean(tempNumReal);
        double[] avgNumImag = getMean(tempNumImag);
        
        double[] avgDenReal = getMean(tempDenReal);
        double[] avgDenImag = getMean(tempDenImag);
        
        double[] tempReal = new double[t1Real.length];
        double[] tempImag = new double[t1Real.length];
        
        for(int i=0; i<t1Real.length; i++) {
            pow = Math.pow(avgDenReal[i], 2.0) + Math.pow(avgDenImag[i], 2);
            tempReal[i] = (avgNumReal[i]*avgDenReal[i] + avgDenImag[i]*avgNumImag[i])/pow;
            tempImag[i] = (avgNumImag[i]*avgDenReal[i] - avgNumReal[i]*avgDenImag[i])/pow;
            t1Real[i] = Math.sqrt(Math.pow(tempReal[i], 2) + Math.pow(tempImag[i], 2));
        }   	
        
    	int replace = doMaxMinReplace(t1Real);
    	
    	
    	System.out.println("Num replaces: "+replace);
    	replace = 0;
    	for(int i=0; i<t1Real.length; i++) {
    	    //System.out.println("Desired index for "+t1Real[i]+" "+(t1Real[i]/t1Precision+1));
    	    t1Real[i] = Math.round(t1Real[i]/t1Precision+1);
    	    if(t1Real[i] > t1Table.length-1) {
    	        t1Real[i] = t1Table.length-1;
    	        replace++;
    	    }
            
            t1Real[i] = t1Table[(int)t1Real[i]];
        }
    	System.out.println("Num replaces: "+replace);
    	
    	try {
            destImage.importData(0, t1Real, true);
        } catch (IOException e) {
            e.printStackTrace();
        }
    	
        FileInfoBase.copyCoreInfo(minImage.getFileInfo(), destImage.getFileInfo());
        
        setImageAttributes(destImage);
    }
    
    /**
     * Reconstructs each inversion time image.
     * 
     * @param each imageReal/imageImage are [xDim*yDim*zDim][numChannel] size
     * @return
     */
    private ModelImage reconstruct(double[][] imageReal,
            double[][] imageImag, String name) {
        ModelImage reconImage = new ModelImage(DataType.DOUBLE, new int[]{xDim,yDim,zDim}, name);
        double[][] squareRealResult = new double[imageReal.length][];
        double[][] squareImagResult = new double[imageImag.length][];
        for(int i=0; i<imageReal.length; i++) {
            squareRealResult[i] = new double[imageReal[i].length];
            squareImagResult[i] = new double[imageImag[i].length];
            for(int j=0; j<imageReal[i].length; j++) {
                squareRealResult[i][j] = imageReal[i][j]*imageReal[i][j] - imageImag[i][j]*imageImag[i][j];
                squareImagResult[i][j] = 2*imageReal[i][j]*imageImag[i][j];
            }
        }
        
        double[] sumReal = new double[squareRealResult.length];
        double[] sumImag = new double[squareImagResult.length];
        for(int i=0; i<squareRealResult.length; i++) {
            sumReal[i] = 0.0;
            sumImag[i] = 0.0;
            for(int j=0; j<squareRealResult[i].length; j++) {
                sumReal[i] += squareRealResult[i][j];
                sumImag[i] += squareImagResult[i][j];
            }
        }
        
        double[] mag = new double[sumReal.length];
        for(int i=0; i<sumReal.length; i++) {
            //get magnitude
            mag[i] = Math.sqrt(sumReal[i]*sumReal[i] + sumImag[i]*sumImag[i]);
            //now get square root
            mag[i] = Math.sqrt(mag[i]);
        }
        try {
            reconImage.importData(0, mag, true);
            setImageAttributes(reconImage);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        
        return reconImage;
    }

    private void setImageAttributes(ModelImage baseImage) {
    	baseImage.setResolutions(new float[]{.6f, .6f, .6f});
        for(int i=0; i<baseImage.getFileInfo().length; i++) {
        	baseImage.getFileInfo(i).setSliceThickness(0);
        	baseImage.getFileInfo(i).setAxisOrientation(new int[]{FileInfoBase.ORI_R2L_TYPE, 
        															FileInfoBase.ORI_A2P_TYPE,
        															FileInfoBase.ORI_I2S_TYPE});
        	baseImage.getFileInfo(i).setImageOrientation(FileInfoBase.AXIAL);
        	baseImage.getFileInfo(i).setModality(FileInfoBase.MAGNETIC_RESONANCE);
        }	//TODO: use enums
	}

	private int doMaxMinReplace(double[] t1Real) {
        int replace = 0;
        for(int i=0; i<t1Real.length; i++) {
            if(t1Real[i] - funcMin > funcMax - funcMin) {
                //System.out.println("MaxReplacing "+t1Real[i]+" with "+(funcMax - funcMin));
                t1Real[i] = funcMax - funcMin;
                replace++;
            } else {
                //System.out.println("MinReplacing "+t1Real[i]+" with "+funcMin);
                replace++;
                t1Real[i] = t1Real[i] - funcMin;
            }
            if(t1Real[i] < 0) {
                t1Real[i] = 0;
            }
        }
        return replace;
    }

    @SuppressWarnings("unused")
    private void searchForValues(double[][] minImageComplexReal,
            double[][] minImageComplexImag, double[][] medImageComplexReal,
            double[][] medImageComplexImag, double[][] maxImageComplexReal,
            double[][] maxImageComplexImag) {
        
        int search = -19262;
        for(int i=0; i<minImageComplexReal.length; i++) {
            for(int j=0; j<minImageComplexReal[i].length; j++) {
                if(minImageComplexReal[i][j] > search-1 &&  minImageComplexReal[i][j] < search+1) {
                    System.out.println("i, j: "+i+", "+j+": "+minImageComplexReal[i][j]);
                }
            }
        }
        System.out.println("Here");
        
    }

    private void rescaleToComplex(ModelImage origImage, double[][] realData, double[][] complexData) {
        double[] realDataTemp = new double[xDim*yDim*zDim*numChannel];
        double[] complexDataTemp = new double[xDim*yDim*zDim*numChannel];
        
        try {
            origImage.exportData(0, xDim*yDim*zDim*numChannel, realDataTemp);
            origImage.exportData(xDim*yDim*zDim*numChannel, xDim*yDim*zDim*numChannel, complexDataTemp);            
            //resultImage.importComplexData(0, realData, complexData, true, false);
            System.out.println("Compare ends "+realDataTemp[realDataTemp.length-1]+" "+complexDataTemp[0]);
        } catch(IOException e) {
            e.printStackTrace();
        }

        int index = 0;
        for(int j=0; j<numChannel; j++) {
            for(int i=0; i<realData.length; i++) {
                realData[i][j] = realDataTemp[index];
                complexData[i][j] = complexDataTemp[index];
                index++;
            }
        }
    }
	
	private double t1Func(double x) {
	    return (Math.exp(-invTimeMax/x)-Math.exp(-invTimeMed/x)) / (Math.exp(-invTimeMed/x)-Math.exp(-invTimeMin/x));
	}
	
	private double[] getMean(double[][] orig) {
	    double[] mean = new double[orig.length];
	    double avg;
	    for(int i=0; i<mean.length; i++) {
	        avg = 0;
	        for(int j=0; j<orig[i].length; j++) {
	            avg += orig[i][j];
	        }
	        avg /= orig[i].length;
	        mean[i] = avg;
	    }
	    return mean;
	}
	
}
