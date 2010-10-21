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
    	
    	double funcMin = t1Val[0], funcMax = t1Val[0];
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
    	
    	System.out.println("New precision value is: "+t1Precision);
    	
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
    	ModelImage minImageComplex = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "minImageComplex");
    	ModelImage medImageComplex = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "medImageComplex");
    	ModelImage maxImageComplex = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "maxImageComplex");
    	ModelImage negPhase = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "negPhaseComplex");
    	
    	ModelImage resultImage = new ModelImage(ModelImage.COMPLEX, new int[]{64,64,44,5}, "maxImageComplex");
    	
    	rescaleToComplex(minImage, minImageComplex);
    	rescaleToComplex(medImage, medImageComplex);
    	rescaleToComplex(maxImage, maxImageComplex);
    	
    	AlgorithmImageCalculator calcAlgo = new AlgorithmImageCalculator(resultImage, medImageComplex, maxImageComplex, AlgorithmImageCalculator.SUBTRACT, 0, false, "");
    	calcAlgo.runAlgorithm();
    	

    	
    	int[] minImageTotal = new int[64*64*44*5];
    	
    	
    	
    	destImage = new ModelImage(ModelImage.DOUBLE, new int[]{64,64,44}, "T1 Result");
    	double t1Num = 0.0;
    	for(int i=0; i<destImage.getSize(); i++) {
    	    t1Num = (maxImage.get(i).doubleValue() - medImage.get(i).doubleValue()) / (medImage.get(i).doubleValue() - minImage.get(i).doubleValue());
    	    //System.out.println("Value: "+t1Num);
            destImage.set(i, t1Num);
        }
    	
    	ViewJFrameImage frame = new ViewJFrameImage(destImage);
    	frame.setVisible(true);
    	fireProgressStateChanged("Message 3D: "+srcImage.getImageName());
	    	
	    	
    }
    
    private void rescaleToComplex(ModelImage origImage, ModelImage resultImage) {
        float[] realData = new float[64*64*44*5];
        float[] complexData = new float[64*64*44*5];
        
        try {
            origImage.exportData(0, 64*64*44*5, realData);
            origImage.exportData(64*64*44*5-1, 64*64*44*5, complexData);            
            resultImage.importComplexData(0, realData, complexData, true, false);
            System.out.println("Compare ends "+realData[realData.length-1]+" "+complexData[0]);
        } catch(IOException e) {
            e.printStackTrace();
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
	
}
