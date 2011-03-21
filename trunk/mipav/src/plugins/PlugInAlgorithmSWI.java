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

import java.util.BitSet;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmFFT2;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

/**
 * This class recalculates mo and dceFullTre to provide better estimates using the inverse Ernst equation
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmSWI extends AlgorithmBase {
    
    private ModelImage magImage;

    private ModelImage phaseImage;

    private double maskThreshold;

    private int roFilterSize;

    
    private int peFilterSize;

    
    private int multFactor;

    /** X dimension length */    
    private int sizeRo;

    /** Y dimension length */
    private int sizePe;
    
    /** Z dimension length */
    private int sizeSs;
    
    private double originRo;
    
    private double originPe;

    /** Whether intermediate images should be displayed during pipeline processing */
    private boolean showInterImages;

    private ModelImage kImage = null;

    private ModelImage iImage = null;

    /** Denotes which pixels undergo SWI processing. */
    private ModelImage brainMask = null;

    private ModelImage kCenterImage = null;

    private ModelImage iFinal = null;

    private ModelImage phaseMask = null;

    /** Denotes whether the algorithm is run in a script/JIST, if so showInterImages just stores images for later access. */
    private boolean inScript = false;

    private ModelImage iCenter = null;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmSWI(boolean inScript, ModelImage resultImage, ModelImage magImage, ModelImage phaseImage, 
            double maskThreshold, int roFilterSize, int peFilterSize, int multFactor, boolean showInterImages) {
        super(resultImage, magImage);
        this.inScript = inScript;
        this.magImage = magImage;
        this.phaseImage = phaseImage;
        this.maskThreshold = maskThreshold;
        this.roFilterSize = roFilterSize;
        this.peFilterSize = peFilterSize;
        this.multFactor = multFactor;
        this.showInterImages = showInterImages;
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
    	
        fireProgressStateChanged(10, "SWI", "Creating complex image...");
    	ModelImage iImage = createiImage(realData, imagData);
    	
    	fireProgressStateChanged(15, "SWI", "Performing FFT...");
    	ModelImage kImage = createkImage(iImage);
    	
    	int upper = (int) Math.pow(2, 128);
    	boolean foundRo = false, foundPe = false;
    	for(int i=1; i<upper; i=i*2) {
    	    if(i > sizeRo) {
    	        sizeRo = i;
    	        foundRo = true;
    	    }
    	    if(i > sizePe) {
    	        sizePe = i;
    	        foundPe = true;
    	    }
    	    if(foundRo && foundPe) {
    	        break;
    	    }
    	}
    	
    	System.out.println("Complex Ro: "+sizeRo);
    	System.out.println("Complex Pe: "+sizePe);
    	
        originRo = (sizeRo - roFilterSize)/2;
        originPe = (sizePe - peFilterSize)/2;
        
        fireProgressStateChanged(25, "SWI", "Creating frequency window...");
    	ModelImage kCenterImage = createKCenterImage(kImage);
    	
    	sizeRo = magImage.getExtents()[0];
        sizePe = magImage.getExtents()[1];
        
        originRo = (sizeRo - roFilterSize)/2;
        originPe = (sizePe - peFilterSize)/2;
        fireProgressStateChanged(45, "SWI", "Running inverse FFT...");
    	ModelImage iCenterImage = runiFFTonKCenter(kCenterImage); //once again 480x480
    	
    	BitSet brainMaskSet = new BitSet(sizeRo*sizePe*sizeSs);
        try {
            brainMask.exportData(0, sizeRo*sizePe*sizeSs, brainMaskSet);
        } catch(Exception e) {
            e.printStackTrace();
        }
    	
        double[] ixRealFinal = new double[sizeRo*sizePe*sizeSs];
        double[] ixImagFinal = new double[sizeRo*sizePe*sizeSs];

        fireProgressStateChanged(55, "SWI", "Dividing by complex image...");
        generateIFinal(iImage, iCenterImage, brainMaskSet, ixRealFinal, ixImagFinal);
        
    	double[] phaseMaskData = new double[sizeRo*sizePe*sizeSs];

    	fireProgressStateChanged(75, "SWI", "Creating phase mask...");
        generatePhaseMask(brainMaskSet, ixRealFinal, ixImagFinal, phaseMaskData);

        fireProgressStateChanged(95, "SWI", "Multiplying by phase mask...");
        destImage = generateMagEnhanced(phaseMaskData, magImage);
    }

    private ModelImage generateMagEnhanced(double[] phaseMaskData, ModelImage magnitude) {
        ModelImage magEnhanced = new ModelImage(ModelImage.DOUBLE, new int[]{sizeRo,sizePe,sizeSs}, "magEnhanced");
        double[] realData = new double[magnitude.getDataSize()];
        try {
            magnitude.exportData(0, magnitude.getDataSize(), realData);
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        
        for(int i=0; i<realData.length; i++) {
            realData[i] = realData[i]*(Math.pow(phaseMaskData[i], multFactor));
        }
        try {
            magEnhanced.importData(0, realData, true);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        if(showInterImages) {
            ModelImage magEnhancedAlg = (ModelImage) magEnhanced.clone();
            magEnhancedAlg.setImageName("magEnhancedAlg");
            if(!inScript) {
                ViewJFrameImage magEnhancedFrame = new ViewJFrameImage(magEnhancedAlg);
                magEnhancedFrame.setVisible(true);
            }
        }
        
        return magEnhanced;
    }

    private ModelImage generatePhaseMask(BitSet brainMaskSet, double[] ixRealFinal, double[] ixImagFinal, double[] phaseMaskData) {
        ModelImage phaseMask = new ModelImage(ModelImage.DOUBLE, new int[]{sizeRo,sizePe,sizeSs}, "phaseMask");

        for(int i=0; i<sizeRo*sizePe*sizeSs; i++) {
            phaseMaskData[i] = 1;
        }
        
        
        for (int i = brainMaskSet.nextSetBit(0); i >= 0; i = brainMaskSet.nextSetBit(i+1)) {
           if(Math.atan2(ixImagFinal[i], ixRealFinal[i]) < 0) {
               phaseMaskData[i] = (float) ((Math.PI + Math.atan2(ixImagFinal[i], ixRealFinal[i])) / Math.PI);
           } 
        }
        
        try {
            phaseMask.importData(0, phaseMaskData, true);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        if(showInterImages) {
            this.phaseMask = phaseMask;
            if(!inScript) {
                ViewJFrameImage phaseMaskFrame = new ViewJFrameImage(phaseMask);
                phaseMaskFrame.setVisible(true);
            }
        }
        
        return phaseMask;
    }

    private ModelImage generateIFinal(ModelImage iImage, ModelImage iCenterImage, BitSet brainMaskSet, double[] ixRealFinal, double[] ixImagFinal) {
        ModelImage iFinal = new ModelImage(ModelImage.DCOMPLEX, new int[]{sizeRo,sizePe,sizeSs}, "iFinal");
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
            ixRealFinal[i] = (ixRealCenter[i]*ixReal[i] + ixImagCenter[i]*ixImag[i])/mag; 
            ixImagFinal[i] = (ixImagCenter[i]*ixReal[i] - ixRealCenter[i]*ixImag[i])/mag;
        }
        
        try {
            iFinal.importDComplexData(0, ixRealFinal, ixImagFinal, true, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        if(showInterImages) {
            this.iFinal = iFinal;
            if(!inScript) {
                ViewJFrameImage iFinalFrame = new ViewJFrameImage(iFinal);
                iFinalFrame.setVisible(true);
            }
        }
        
        return iFinal;
    }

    private ModelImage runiFFTonKCenter(ModelImage kCenterImage) {
        ModelImage iFFTDest = new ModelImage(ModelImage.DCOMPLEX, new int[]{sizeRo, sizePe, sizeSs}, "ifftDest");
        kCenterImage.setImage25D(true);
        iFFTDest.setImage25D(true);
        boolean complexInverse = true;
        AlgorithmFFT2 fft2 = new AlgorithmFFT2(iFFTDest, kCenterImage, AlgorithmFFT2.INVERSE, false, false, true,
        		                             complexInverse);
        fft2.run();
        fft2.finalize();
        float[] realData = new float[sizeRo*sizePe*sizeSs], imagData = new float[sizeRo*sizePe*sizeSs];
        try {
            iFFTDest.exportComplexData(0, sizeRo*sizePe*sizeSs, realData, imagData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        ModelImage iCenter = new ModelImage(ModelImage.COMPLEX, new int[]{sizeRo, sizePe, sizeSs}, "ixcenter");
        try {
            iCenter.importComplexData(0, realData, imagData, true, false);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        if(showInterImages) {
            this.iCenter = iCenter;
            if(!inScript) {
                ViewJFrameImage iNewCenterFrame = new ViewJFrameImage(iCenter);  //ixcenter should now be same as ifftDest
                iNewCenterFrame.setVisible(true);
            }
        }
        
        return iCenter;
    }

    private ModelImage createKCenterImage(ModelImage kImage) {
        ModelImage kCenterImage = (ModelImage) kImage.clone();
        int xDim = kCenterImage.getExtents()[0];
        int yDim = kCenterImage.getExtents()[1];
        kCenterImage.setImageName("kCenterImage");
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                if(i < originRo+1 || i > originRo+roFilterSize) {
                    //System.out.println("Coord: ("+i+", "+j+")");
                    for(int k=0; k<sizeSs; k++) {
                        kCenterImage.set(2*(k*(xDim*yDim) + j*(xDim) + i), 0);
                        kCenterImage.set(2*(k*(xDim*yDim) + j*(xDim) + i)+1, 0);
                    }
                } 
            }
        }
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                if(j < originPe+1 || j > originPe + peFilterSize) {
                    //System.out.println("Coord: ("+i+", "+j+")");
                    for(int k=0; k<sizeSs; k++) {
                        kCenterImage.set(2*(k*(xDim*yDim) + j*(xDim) + i), 0);
                        kCenterImage.set(2*(k*(xDim*yDim) + j*(xDim) + i)+1, 0);
                    }
                } 
            }
        }
        
        if(showInterImages) {
            this.kCenterImage = kCenterImage;
            if(!inScript) {
                ViewJFrameImage kCenterFrame = new ViewJFrameImage(kCenterImage);
                kCenterFrame.setVisible(true);
            }
        }
        
        return kCenterImage;
    }

    private ModelImage createkImage(ModelImage iImage) {
        ModelImage kImage = new ModelImage(ModelImage.DCOMPLEX, new int[]{sizeRo,sizePe,sizeSs}, "kData");
        
        //kImage is now at 512x512
        boolean complexInverse = false;
        AlgorithmFFT2 fft = new AlgorithmFFT2(kImage, iImage, AlgorithmFFT2.FORWARD, false, false, true,
        		                            complexInverse);
        iImage.setImage25D(true);
        kImage.setImage25D(true);
        fft.run();
        fft.finalize();
        
        if(showInterImages) {
            this.kImage = kImage;
            if(!inScript) {
                ViewJFrameImage kFrame = new ViewJFrameImage(kImage);
                kFrame.setVisible(true);
            }
        }
        
        return kImage;
    }

    private ModelImage createiImage(double[] realData, double[] imagData) {
        ModelImage iImage  = new ModelImage(ModelImage.DCOMPLEX, new int[]{sizeRo,sizePe,sizeSs}, "iData");

        try {
            iImage.importDComplexData(0, realData, imagData, true, false);
            iImage.setOriginalExtents(iImage.getExtents());
        } catch (IOException e2) {
            // TODO Auto-generated catch block
            e2.printStackTrace();
        }
        
        if(showInterImages) {
            this.iImage = iImage;
            if(!inScript) {
                ViewJFrameImage iFrame = new ViewJFrameImage(iImage);
                iFrame.setVisible(true);
            }
        }
        
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
        
        if(showInterImages) {
            this.brainMask = brainMask;
            if(!inScript) {
                ViewJFrameImage brainFrame = new ViewJFrameImage(brainMask);
                brainFrame.setVisible(true);
            }
        }
        
        return brainMask;
    }

    private void rescaleToComplex(ModelImage magImage, ModelImage phaseImage, double[] realData, double[] imagData) {
        double[] magImageTemp = new double[realData.length];
        double[] phaseImageTemp = new double[imagData.length];
                
        try {
            magImage.exportData(0, realData.length, magImageTemp);
            phaseImage.exportData(0, imagData.length, phaseImageTemp);            
        } catch(IOException e) {
            e.printStackTrace();
        }

        for(int i=0; i<realData.length; i++) {
            realData[i] = magImageTemp[i]*Math.cos(phaseImageTemp[i]);
            imagData[i] = magImageTemp[i]*Math.sin(phaseImageTemp[i]);
        }
    }

    public ModelImage getkImage() {
        return kImage;
    }

    public ModelImage getiImage() {
        return iImage;
    }

    public ModelImage getBrainMask() {
        return brainMask;
    }

    public ModelImage getkCenterImage() {
        return kCenterImage;
    }

    public ModelImage getiFinal() {
        return iFinal;
    }

    public ModelImage getPhaseMask() {
        return phaseMask;
    }

    public ModelImage getiCenter() {
        return iCenter;
    }
	
}
