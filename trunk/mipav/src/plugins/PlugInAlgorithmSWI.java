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
import java.util.BitSet;
import java.util.Random;
import java.util.TreeMap;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmFFT;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
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

public class PlugInAlgorithmSWI extends AlgorithmBase {
    
	  	/** X dimension of the image */
    private int xDim;

	    /** Y dimension of the image */
    private int yDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    private ModelImage magImage;

    private ModelImage phaseImage;

    private double maskThreshold;

    private int roFilterSize;

    private int peFilterSize;

    private int multFactor;

    private int sizeRo;

    private int sizePe;

    private int sizeSs;

    private double originRo;

    private double originPe;

    private AlgorithmAddMargins imageMarginsAlgo;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmSWI(ModelImage resultImage, ModelImage magImage, ModelImage phaseImage, 
            double maskThreshold, int roFilterSize, int peFilterSize, int multFactor) {
        super(resultImage, magImage);
        this.magImage = magImage;
        this.phaseImage = phaseImage;
        this.maskThreshold = maskThreshold;
        this.roFilterSize = roFilterSize;
        this.peFilterSize = peFilterSize;
        this.multFactor = multFactor;
        
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
    	
    	System.out.println("SWI processing here");
    	sizeRo = magImage.getExtents()[0];
    	sizePe = magImage.getExtents()[1];
    	sizeSs = magImage.getExtents()[2];
    	
    	originRo = (sizeRo - roFilterSize)/2;
    	originPe = (sizePe - peFilterSize)/2;
    	
    	ModelImage brainMask = createBrainMask();
    	
    	double[] realData = new double[sizeRo*sizePe*sizeSs];
        double[] imagData = new double[sizeRo*sizePe*sizeSs];
        
        rescaleToComplex(magImage, phaseImage, realData, imagData);
    	
    	ModelImage iImage = createiImage(realData, imagData);
    	
    	ModelImage kImage = createkImage(iImage);
    	
    	ModelImage kCenterImage = createKCenterImage(kImage);
    
    	ModelImage iCenterImage = padiImage(iImage);
        
    	iCenterImage = runFFToniCenter(iCenterImage, kCenterImage);
        
    	BitSet brainMaskSet = new BitSet(sizeRo*sizePe*sizeSs);
        try {
            brainMask.exportData(0, sizeRo*sizePe*sizeSs, brainMaskSet);
        } catch(Exception e) {
            e.printStackTrace();
        }
    	
        float[] ixRealFinal = new float[sizeRo*sizePe*sizeSs];
        float[] ixImagFinal = new float[sizeRo*sizePe*sizeSs];
        
    	ModelImage iFinal = generateIFinal(iImage, iCenterImage, brainMaskSet, ixRealFinal, ixImagFinal);
        
    	float[] phaseMaskData = new float[sizeRo*sizePe*sizeSs];
        ModelImage phaseMask = generatePhaseMask(brainMaskSet, ixRealFinal, ixImagFinal, phaseMaskData);

        ModelImage magEnhanced = generateMagEnhanced(phaseMaskData, realData);
    }
    
    private ModelImage generateMagEnhanced(float[] phaseMaskData, double[] realData) {
        ModelImage magEnhanced = new ModelImage(ModelImage.FLOAT, new int[]{sizeRo,sizePe,sizeSs}, "magEnhanced");
        for(int i=0; i<realData.length; i++) {
            realData[i] = realData[i]*(Math.pow(phaseMaskData[i], multFactor));
        }
        try {
            magEnhanced.importData(0, realData, true);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        ViewJFrameImage magEnhancedFrame = new ViewJFrameImage(magEnhanced);
        magEnhancedFrame.setVisible(true);
        
        return magEnhanced;
    }

    private ModelImage generatePhaseMask(BitSet brainMaskSet, float[] ixRealFinal, float[] ixImagFinal, float[] phaseMaskData) {
        ModelImage phaseMask = new ModelImage(ModelImage.FLOAT, new int[]{sizeRo,sizePe,sizeSs}, "phaseMask");
        
        for(int i=0; i<sizeRo*sizePe*sizeSs; i++) {
            phaseMaskData[i] = 1;
        }
        
        
        for (int i = brainMaskSet.nextSetBit(0); i >= 0; i = brainMaskSet.nextSetBit(i+1)) {
           if(Math.atan(ixRealFinal[i]/ixImagFinal[i]) < 0) {
               phaseMaskData[i] = (float) ((Math.PI + Math.atan(ixRealFinal[i]/ixImagFinal[i])) / Math.PI);
           } 
        }
        
        try {
            phaseMask.importData(0, phaseMaskData, true);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        ViewJFrameImage phaseMaskFrame = new ViewJFrameImage(phaseMask);
        phaseMaskFrame.setVisible(true);
        
        return phaseMask;
    }

    private ModelImage generateIFinal(ModelImage iImage, ModelImage iCenterImage, BitSet brainMaskSet, float[] ixRealFinal, float[] ixImagFinal) {
        ModelImage iFinal = new ModelImage(ModelImage.COMPLEX, new int[]{sizeRo,sizePe,sizeSs}, "complexData");
        
        double[] ixReal = new double[sizeRo*sizePe*sizeSs];
        double[] ixImag = new double[sizeRo*sizePe*sizeSs];
        double[] ixRealCenter = new double[sizeRo*sizePe*sizeSs];
        double[] ixImagCenter = new double[sizeRo*sizePe*sizeSs];
        try {
            iImage.exportDComplexData(0, sizeRo*sizePe*sizeSs, ixReal, ixImag);
            iCenterImage.exportDComplexData(0, sizeRo*sizePe*sizeSs, ixRealCenter, ixImagCenter);
            
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }      
        
        double mag = 0.0;
        //this is equivalent to multiplying by the brain mask because calculation is occurring only at every set
        //bit of the brainMask
        for (int i = brainMaskSet.nextSetBit(0); i >= 0; i = brainMaskSet.nextSetBit(i+1)) {
            mag = Math.pow(ixReal[i], 2) + Math.pow(ixImag[i], 2);
            ixRealFinal[i] = (float) ((ixRealCenter[i]*ixReal[i] + ixImagCenter[i]*ixImag[i])/mag);
            ixImagFinal[i] = (float) ((ixImagCenter[i]*ixReal[i] - ixRealCenter[i]*ixImag[i])/mag);
        }
        
        try {
            iFinal.importComplexData(0, ixRealFinal, ixImagFinal, true, false);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        ViewJFrameImage iFinalFrame = new ViewJFrameImage(iFinal);
        iFinalFrame.setVisible(true);
        
        return iFinal;
    }

    private ModelImage runFFToniCenter(ModelImage iCenterImage, ModelImage kCenterImage) {
        AlgorithmFFT fft2 = new AlgorithmFFT(iCenterImage, new ModelImage(ModelImage.COMPLEX, new int[]{512,512,40}, "complexData"), AlgorithmFFT.INVERSE, false, false, true);
        fft2.run();
        
        ViewJFrameImage iCenterFrame = new ViewJFrameImage(iCenterImage);
        iCenterFrame.setVisible(true);
        
        return iCenterImage;
    }

    private ModelImage padiImage(ModelImage iImage) {
        ModelImage iCenterImage = (ModelImage) iImage.clone(); //also needs to be 480x480
        iCenterImage.setImageName("iCenterImage");
        imageMarginsAlgo = new AlgorithmAddMargins(iCenterImage, 
                new int[]{16,16}, new int[]{16,16}, new int[]{0, 0});
        imageMarginsAlgo.setPadValue(new float[]{0});
        
        imageMarginsAlgo.runAlgorithm();
        iCenterImage = imageMarginsAlgo.getSrcImage();
        
        ViewJFrameImage iCenterFrameBeforeFFT = new ViewJFrameImage(iCenterImage);
        iCenterFrameBeforeFFT.setVisible(true);
        
        return iCenterImage;
    }

    private ModelImage createKCenterImage(ModelImage kImage) {
        ModelImage kCenterImage = (ModelImage) kImage.clone();
        kCenterImage.setImageName("kCenterImage");
        for(int i=0; i<sizeRo; i++) {
            if(i < originRo+1 || i > originRo+roFilterSize) {
                for(int j=0; j<sizePe; j++) {
                    if(j < originPe+1 || j > originPe + peFilterSize) {
                        for(int k=0; k<sizeSs; k++) {
                                kCenterImage.set(i, j, k, 0);
                        }
                    }
                }
            }
        }
        
        ViewJFrameImage kCenterFrame = new ViewJFrameImage(kCenterImage);
        kCenterFrame.setVisible(true);
        
        return kCenterImage;
    }

    private ModelImage createkImage(ModelImage iImage) {
        ModelImage kImage = new ModelImage(ModelImage.COMPLEX, new int[]{sizeRo,sizePe,sizeSs}, "kData");
        
        int a = 0;
        
        //kImage is now at 512x512
        AlgorithmFFT fft = new AlgorithmFFT(kImage, iImage, AlgorithmFFT.FORWARD, false, false, true);
        fft.run();
        
        ViewJFrameImage kFrame = new ViewJFrameImage(kImage);
        kFrame.setVisible(true);
        
        return kImage;
    }

    private ModelImage createiImage(double[] realData, double[] imagData) {
        ModelImage iImage  = new ModelImage(ModelImage.COMPLEX, new int[]{sizeRo,sizePe,sizeSs}, "iData");

        try {
            iImage.importDComplexData(0, realData, imagData, true, false);
            iImage.setOriginalExtents(iImage.getExtents());
        } catch (IOException e2) {
            // TODO Auto-generated catch block
            e2.printStackTrace();
        }
        
        ViewJFrameImage iFrame = new ViewJFrameImage(iImage);
        iFrame.setVisible(true);
        
        return iImage;
    }

    private ModelImage createBrainMask() {
        ModelImage brainMask = new ModelImage(ModelImage.BOOLEAN, new int[]{sizeRo,sizePe,sizeSs}, "brainMask");
        
        for(int i=0; i<sizeRo; i++) {
            for(int j=0; j<sizePe; j++) {
                for(int k=0; k<sizeSs; k++) {
                    if(magImage.getDouble(i, j, k) > maskThreshold) {
                        brainMask.set(i, j, k, true);
                    }
                }
            }
        }
        brainMask.calcMinMax();
        
        ViewJFrameImage brainFrame = new ViewJFrameImage(brainMask);
        brainFrame.setVisible(true);
        
        return brainMask;
    }

    private void rescaleToComplex(ModelImage magImage, ModelImage phaseImage, double[] realData, double[] complexData) {
        double[] magImageTemp = new double[realData.length];
        double[] phaseImageTemp = new double[realData.length];
                
        try {
            magImage.exportData(0, realData.length, magImageTemp);
            phaseImage.exportData(0, realData.length, phaseImageTemp);            
        } catch(IOException e) {
            e.printStackTrace();
        }

        for(int i=0; i<realData.length; i++) {
            realData[i] = magImageTemp[i]*Math.sin(phaseImageTemp[i]);
            complexData[i] = magImageTemp[i]*Math.cos(phaseImageTemp[i]);
        }
    }
        
	private void init() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
    }
	
}
