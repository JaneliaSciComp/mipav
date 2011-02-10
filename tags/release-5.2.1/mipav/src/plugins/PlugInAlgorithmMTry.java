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

import java.io.BufferedReader;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.TreeMap;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * This class recalculates mo and dceFullTre to provide better estimates using the inverse Ernst equation
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmMTry extends AlgorithmBase {
    
	  	/** X dimension of the image */
    private int xDim;

	    /** Y dimension of the image */
    private int yDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    private ModelImage minImage;

    private ModelImage medImage;

    private ModelImage maxImage;

    private double t1Min;

    private double t1Max;

    private double precision;

    private double invTimeMin;

    private double invTimeMed;

    private double invTimeMax;

    private double funcMin;

    private double funcMax;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmMTry(ModelImage resultImage, ModelImage minImage, ModelImage medImage, ModelImage maxImage, 
            double t1Min, double t1Max, double precision, double invTimeMin, double invTimeMed, double invTimeMax) {
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
        
        init();
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
    
	//  ~ Methods --------------------------------------------------------------------------------------------------------
    
    private void calc2D() {
    	fireProgressStateChanged("Message 2D: "+srcImage.getImageName());
     
    	MipavUtil.displayError("This algorithm is not yet designed for 2D images.");
    }
    
    private void calc3D() {
    	
    	System.out.println("4d processing here");
    	
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
    	
    	//Reshape the images as three complex images
    	//ModelImage minImageComplex = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "minImageComplex");
    	//ModelImage medImageComplex = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "medImageComplex");
    	//ModelImage maxImageComplex = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "maxImageComplex");
    	//ModelImage negPhase = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "negPhaseComplex");
    	
    	ModelImage resultImage = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "maxImageComplex");
    	
    	float[][] minImageComplexReal = new float[64*64*44][5], minImageComplexImag = new float[64*64*44][5], 
    	            medImageComplexReal = new float[64*64*44][5], medImageComplexImag = new float[64*64*44][5],
    	            maxImageComplexReal = new float[64*64*44][5], maxImageComplexImag = new float[64*64*44][5], 
    	            negPhaseReal = new float[64*64*44][5], negPhaseImag = new float[64*64*44][5];
    	
    	rescaleToComplex(minImage, minImageComplexReal, minImageComplexImag);
    	rescaleToComplex(medImage, medImageComplexReal, medImageComplexImag);
    	rescaleToComplex(maxImage, maxImageComplexReal, maxImageComplexImag);
    	rescaleToComplex(maxImage, negPhaseReal, negPhaseImag);
    	
    	//searchForValues(minImageComplexReal, minImageComplexImag, medImageComplexReal, medImageComplexImag, maxImageComplexReal, maxImageComplexImag);
    	
    	
    	
    	/*double[] maxImageComplexReal = new double[maxImageComplex.getSize()];
    	double[] maxImageComplexImag = new double[maxImageComplex.getSize()];
    	double[] negPhaseReal = new double[maxImageComplex.getSize()];
    	double[] negPhaseImag = new double[maxImageComplex.getSize()];
    	try {
            maxImageComplex.exportDComplexData(0, maxImageComplex.getSize(), maxImageComplexRReceived	Subject	From	Size	Categories	
1:05 PM	DCO Presentation Monday (11/22) Service Desk Knowledge Management and Change Management	CIT Communications Office (NIH/CIT)	11 KB		eal, maxImageComplexImag);
        } catch (IOException e) {
            e.printStackTrace();
        }*/
    	
        double num, pow, mag;
        for(int i=0; i<maxImageComplexReal.length; i++) {
            for(int j=0; j<maxImageComplexReal[i].length; j++) {
                pow = Math.pow(maxImageComplexReal[i][j], 2.0) + Math.pow(maxImageComplexImag[i][j], 2.0);
                mag = Math.sqrt(pow);
                negPhaseReal[i][j] = (float) ((mag*maxImageComplexReal[i][j])/pow);
                negPhaseImag[i][j] = (float) ((-mag*maxImageComplexImag[i][j])/pow);
            }
        }
        
        float[] t1Real = new float[64*64*44];
        float[][] tempNumReal = new float[64*64*44][5];
        float[][] tempNumImag = new float[64*64*44][5];
        float[][] tempDenReal = new float[64*64*44][5];
        float[][] tempDenImag = new float[64*64*44][5];
        
        float realFactor, imagFactor;
        
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
        int k = 1;
        float[] avgNumReal = getMean(tempNumReal);
        float[] avgNumImag = getMean(tempNumImag);
        
        float[] avgDenReal = getMean(tempDenReal);
        float[] avgDenImag = getMean(tempDenImag);
        
        float[] tempReal = new float[t1Real.length];
        float[] tempImag = new float[t1Real.length];
        
        for(int i=0; i<t1Real.length; i++) {
            pow = Math.pow(avgDenReal[i], 2.0) + Math.pow(avgDenImag[i], 2);
            tempReal[i] = (float) ((avgNumReal[i]*avgDenReal[i] + avgDenImag[i]*avgNumImag[i])/pow);
            tempImag[i] = (float) ((avgNumImag[i]*avgDenReal[i] - avgNumReal[i]*avgDenImag[i])/pow);
            t1Real[i] = (float) Math.sqrt(Math.pow(tempReal[i], 2) + Math.pow(tempImag[i], 2));
        }   	
            
    	
    	destImage = new ModelImage(ModelImage.FLOAT, new int[]{64,64,44}, "T1 Result");
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
            
            t1Real[i] = (float) t1Table[(int)t1Real[i]];
        }
    	System.out.println("Num replaces: "+replace);
    	
    	try {
            destImage.importData(0, t1Real, true);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    	
        FileInfoBase.copyCoreInfo(minImage.getFileInfo(), destImage.getFileInfo());
        
        for(int i=0; i<destImage.getFileInfo().length; i++) {
            destImage.getFileInfo(i).setSliceThickness(minImage.getFileInfo(i).getSliceThickness());
        }
        
    	ViewJFrameImage frame = new ViewJFrameImage(destImage);
    	frame.setVisible(true);
    	fireProgressStateChanged("Message 3D: "+srcImage.getImageName());
	    	
	    	
    }
    
    private int doMaxMinReplace(float[] t1Real) {
        int replace = 0;
        for(int i=0; i<t1Real.length; i++) {
            if(t1Real[i] - funcMin > funcMax - funcMin) {
                //System.out.println("MaxReplacing "+t1Real[i]+" with "+(funcMax - funcMin));
                t1Real[i] = (float) (funcMax - funcMin);
                replace++;
            } else {
                //System.out.println("MinReplacing "+t1Real[i]+" with "+funcMin);
                replace++;
                t1Real[i] = (float) (t1Real[i] - funcMin);
            }
            if(t1Real[i] < 0) {
                t1Real[i] = 0;
            }
        }
        return replace;
    }

    private void searchForValues(float[][] minImageComplexReal,
            float[][] minImageComplexImag, float[][] medImageComplexReal,
            float[][] medImageComplexImag, float[][] maxImageComplexReal,
            float[][] maxImageComplexImag) {
        
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

    private void rescaleToComplex(ModelImage origImage, float[][] realData, float[][] complexData) {
        float[] realDataTemp = new float[64*64*44*5];
        float[] complexDataTemp = new float[64*64*44*5];
        
        try {
            origImage.exportData(0, 64*64*44*5, realDataTemp);
            origImage.exportData(64*64*44*5, 64*64*44*5, complexDataTemp);            
            //resultImage.importComplexData(0, realData, complexData, true, false);
            System.out.println("Compare ends "+realDataTemp[realDataTemp.length-1]+" "+complexDataTemp[0]);
        } catch(IOException e) {
            e.printStackTrace();
        }

        int index = 0;
        for(int j=0; j<5; j++) {
            for(int i=0; i<realData.length; i++) {
                realData[i][j] = realDataTemp[index];
                complexData[i][j] = complexDataTemp[index];
                index++;
            }
        }
    }
        
	private void init() {
    xDim = srcImage.getExtents()[0];
    yDim = srcImage.getExtents()[1];
    sliceSize = xDim * yDim;
    }
	
	private double t1Func(double x) {
	    return (Math.exp(-invTimeMax/x)-Math.exp(-invTimeMed/x)) / (Math.exp(-invTimeMed/x)-Math.exp(-invTimeMin/x));
	}
	
	private float[] getMean(float[][] orig) {
	    float[] mean = new float[orig.length];
	    float avg;
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
