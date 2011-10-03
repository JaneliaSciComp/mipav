package gov.nih.mipav.model.algorithms.filters;
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

import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * This class recalculates mo and dceFullTre to provide better estimates using the inverse Ernst equation
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class AlgorithmSWI extends AlgorithmBase {
    
    private ModelImage magImage;

    private ModelImage phaseImage;

    private double maskThreshold;

    private int xFilterSize;
    
    private int yFilterSize;
    
    private int multFactor;

    /** X dimension length */    
    private int xDim;

    /** Y dimension length */
    private int yDim;
    
    /** Z dimension length */
    private int zDim;
    
    private double xFilterTopLeft;
    
    private double yFilterTopLeft;

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
    public AlgorithmSWI(boolean inScript, ModelImage resultImage, ModelImage magImage, ModelImage phaseImage, 
            double maskThreshold, int roFilterSize, int peFilterSize, int multFactor, boolean showInterImages) {
        super(resultImage, magImage);
        System.out.println("Algorithm is initialized");
        this.inScript = inScript;
        this.magImage = magImage;
        this.phaseImage = phaseImage;
        this.maskThreshold = maskThreshold;
        this.xFilterSize = roFilterSize;
        this.yFilterSize = peFilterSize;
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
        System.out.println("Algorithm is running");
        if(srcImage.getNDims() < 3) {
    		calc2D();
        } else {
        	calc3D();
        }
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()
    
    private void calc2D() {
    	fireProgressStateChanged("Message 2D: "+srcImage.getImageName());
     
    	MipavUtil.displayError("This algorithm is not yet designed for 2D images.");
    }
    
    private void calc3D() {
    	
    	System.out.println("SWI processing here2");
    	xDim = magImage.getExtents()[0];
    	yDim = magImage.getExtents()[1];
    	zDim = magImage.getExtents()[2];
    	
    	xFilterTopLeft = (xDim - xFilterSize)/2;
    	yFilterTopLeft = (yDim - yFilterSize)/2;
    	
    	ModelImage brainMask = createBrainMask();
    	
    	double[] realData = new double[xDim*yDim*zDim];
        double[] imagData = new double[xDim*yDim*zDim];
        
        rescaleToComplex(magImage, phaseImage, realData, imagData);
    	
        fireProgressStateChanged(10, "SWI", "Creating complex image...");
    	ModelImage iImage = createiImage(realData, imagData);
    	
    	fireProgressStateChanged(15, "SWI", "Performing FFT...");
    	ModelImage kImage = createkImage(iImage);
    	
    	int upper = (int) Math.pow(2, 128);
    	boolean foundRo = false, foundPe = false;
    	for(int i=1; i<upper; i=i*2) {
    	    if(i >= xDim) {
    	        xDim = i;
    	        foundRo = true;
    	    }
    	    if(i >= yDim) {
    	        yDim = i;
    	        foundPe = true;
    	    }
    	    if(foundRo && foundPe) {
    	        break;
    	    }
    	}
    	
    	System.out.println("Complex Ro: "+xDim);
    	System.out.println("Complex Pe: "+yDim);
    	
        xFilterTopLeft = (xDim - xFilterSize)/2;
        yFilterTopLeft = (yDim - yFilterSize)/2;
        
        fireProgressStateChanged(25, "SWI", "Creating frequency window...");
    	ModelImage kCenterImage = createKCenterImage(kImage);
    	ViewUserInterface.getReference().unRegisterImage(kCenterImage);
    	
    	xDim = magImage.getExtents()[0];
        yDim = magImage.getExtents()[1];
        
        xFilterTopLeft = (xDim - xFilterSize)/2;
        yFilterTopLeft = (yDim - yFilterSize)/2;
        fireProgressStateChanged(45, "SWI", "Running inverse FFT...");
    	ModelImage iCenterImage = runiFFTonKCenter(kCenterImage); //once again 480x480
    	
    	BitSet brainMaskSet = new BitSet(xDim*yDim*zDim);
        try {
            brainMask.exportData(0, xDim*yDim*zDim, brainMaskSet);
        } catch(Exception e) {
            e.printStackTrace();
        }
    	
        double[] ixRealFinal = new double[xDim*yDim*zDim];
        double[] ixImagFinal = new double[xDim*yDim*zDim];

        fireProgressStateChanged(55, "SWI", "Dividing by complex image...");
        generateIFinal(iImage, iCenterImage, brainMaskSet, ixRealFinal, ixImagFinal);

    	double[] phaseMaskData = new double[xDim*yDim*zDim];

    	fireProgressStateChanged(75, "SWI", "Creating phase mask...");
        generatePhaseMask(brainMaskSet, ixRealFinal, ixImagFinal, phaseMaskData);

        fireProgressStateChanged(95, "SWI", "Multiplying by phase mask...");
        destImage = generateMagEnhanced(phaseMaskData, magImage);
    }

    private ModelImage generateMagEnhanced(double[] phaseMaskData, ModelImage magnitude) {
        ModelImage magEnhanced = new ModelImage(ModelImage.DOUBLE, new int[]{xDim,yDim,zDim}, "magEnhancedFINAL");
        JDialogBase.updateFileInfo(magImage, magEnhanced);
        double[] realData = new double[magnitude.getDataSize()];
        try {
            magnitude.exportData(0, magnitude.getDataSize(), realData);
        } catch (IOException e1) {
            e1.printStackTrace();
        }
        
        for(int i=0; i<realData.length; i++) {
            realData[i] = realData[i]*(Math.pow(phaseMaskData[i], multFactor));
        }
        try {
            magEnhanced.importData(0, realData, false);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        if(showInterImages) {
            ModelImage magEnhancedAlg = (ModelImage) magEnhanced.clone("magEnhancedAlg");
            JDialogBase.updateFileInfo(magImage, magEnhancedAlg);
            if(!inScript) {
                ViewJFrameImage magEnhancedFrame = new ViewJFrameImage(magEnhancedAlg);
                magEnhancedFrame.setVisible(true);
            }
        } else {
        	ViewUserInterface.getReference().unRegisterImage(magEnhanced);
        }
        
        return magEnhanced;
    }

    private ModelImage generatePhaseMask(BitSet brainMaskSet, double[] ixRealFinal, double[] ixImagFinal, double[] phaseMaskData) {
        ModelImage phaseMask = new ModelImage(ModelImage.DOUBLE, new int[]{xDim,yDim,zDim}, "phaseMask");
        JDialogBase.updateFileInfo(magImage, phaseMask);
        
        for(int i=0; i<xDim*yDim*zDim; i++) {
            phaseMaskData[i] = 1;
        }
        
        
        for (int i = brainMaskSet.nextSetBit(0); i >= 0; i = brainMaskSet.nextSetBit(i+1)) {
           if(Math.atan2(ixImagFinal[i], ixRealFinal[i]) < 0) {
               phaseMaskData[i] = (float) ((Math.PI + Math.atan2(ixImagFinal[i], ixRealFinal[i])) / Math.PI);
           } 
        }
        
        try {
            phaseMask.importData(0, phaseMaskData, false);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        if(showInterImages) {
            this.phaseMask = phaseMask;
            if(!inScript) {
                ViewJFrameImage phaseMaskFrame = new ViewJFrameImage(phaseMask);
                phaseMaskFrame.setVisible(true);
            }
        } else {
        	ViewUserInterface.getReference().unRegisterImage(phaseMask);
        }
        
        return phaseMask;
    }

    private ModelImage generateIFinal(ModelImage iImage, ModelImage iCenterImage, BitSet brainMaskSet, double[] ixRealFinal, double[] ixImagFinal) {
        ModelImage iFinal = new ModelImage(ModelImage.DCOMPLEX, new int[]{xDim,yDim,zDim}, "iFinal");
        JDialogBase.updateFileInfo(magImage, iFinal);
        double[] ixReal = new double[xDim*yDim*zDim];
        double[] ixImag = new double[xDim*yDim*zDim];
        double[] ixRealCenter = new double[xDim*yDim*zDim];
        double[] ixImagCenter = new double[xDim*yDim*zDim];
        try {
            iImage.exportDComplexData(0, xDim*yDim*zDim, ixReal, ixImag);
            iCenterImage.exportDComplexData(0, xDim*yDim*zDim, ixRealCenter, ixImagCenter);
            
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
            iFinal.importDComplexData(0, ixRealFinal, ixImagFinal, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        if(showInterImages) {
            this.iFinal = iFinal;
            if(!inScript) {
                ViewJFrameImage iFinalFrame = new ViewJFrameImage(iFinal);
                iFinalFrame.setVisible(true);
            }
        } else {
        	ViewUserInterface.getReference().unRegisterImage(iFinal);
        }
        
        return iFinal;
    }

    private ModelImage runiFFTonKCenter(ModelImage kCenterImage) {
        ModelImage iFFTDest = new ModelImage(ModelImage.DCOMPLEX, new int[]{xDim, yDim, zDim}, "ifftDest");
        JDialogBase.updateFileInfo(magImage, iFFTDest);
        kCenterImage.setImage25D(true);
        iFFTDest.setImage25D(true);
        boolean complexInverse = true;
        AlgorithmFFT fft2 = new AlgorithmFFT(iFFTDest, kCenterImage, AlgorithmFFT.INVERSE, false, false, true,
        		                             complexInverse);
        fft2.run();
        fft2.finalize();
        float[] realData = new float[xDim*yDim*zDim], imagData = new float[xDim*yDim*zDim];
        try {
            iFFTDest.exportComplexData(0, xDim*yDim*zDim, realData, imagData);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        ModelImage iCenter = new ModelImage(ModelImage.COMPLEX, new int[]{xDim, yDim, zDim}, "ixcenter");
        JDialogBase.updateFileInfo(magImage, iCenter);
        try {
            iCenter.importComplexData(0, realData, imagData, false, false);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        if(showInterImages) {
            this.iCenter = iCenter;
            if(!inScript) {
                ViewJFrameImage iNewCenterFrame = new ViewJFrameImage(iCenter);  //ixcenter should now be same as ifftDest
                iNewCenterFrame.setVisible(true);
            }
        } else {
        	ViewUserInterface.getReference().unRegisterImage(iFFTDest);
        	ViewUserInterface.getReference().unRegisterImage(iCenter);
        }
        
        return iCenter;
    }

    private ModelImage createKCenterImage(ModelImage kImage) {
        ModelImage kCenterImage = (ModelImage) kImage.clone("kCenterImage222");
        JDialogBase.updateFileInfo(magImage, kCenterImage);
        int xDim = kCenterImage.getExtents()[0];
        int yDim = kCenterImage.getExtents()[1];
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                if(i < xFilterTopLeft+1 || i > xFilterTopLeft+xFilterSize) {
                    //System.out.println("Coord: ("+i+", "+j+")");
                    for(int k=0; k<zDim; k++) {
                        kCenterImage.set(2*(k*(xDim*yDim) + j*(xDim) + i), 0);
                        kCenterImage.set(2*(k*(xDim*yDim) + j*(xDim) + i)+1, 0);
                    }
                } 
            }
        }
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                if(j < yFilterTopLeft+1 || j > yFilterTopLeft + yFilterSize) {
                    //System.out.println("Coord: ("+i+", "+j+")");
                    for(int k=0; k<zDim; k++) {
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
        } else {
        	ViewUserInterface.getReference().unRegisterImage(kCenterImage);
        }
        
        return kCenterImage;
    }

    private ModelImage createkImage(ModelImage iImage) {
        ModelImage kImage = new ModelImage(ModelImage.DCOMPLEX, new int[]{xDim,yDim,zDim}, "kData");
        JDialogBase.updateFileInfo(magImage, kImage);
        
        //kImage is now at 512x512
        boolean complexInverse = false;
        AlgorithmFFT fft = new AlgorithmFFT(kImage, iImage, AlgorithmFFT.FORWARD, false, false, true,
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
        } else {
        	ViewUserInterface.getReference().unRegisterImage(kImage);
        }
        
        return kImage;
    }

    private ModelImage createiImage(double[] realData, double[] imagData) {
        
    	ModelImage iImage  = new ModelImage(ModelImage.DCOMPLEX, new int[]{xDim,yDim,zDim}, "iData");
    	JDialogBase.updateFileInfo(magImage, iImage);
    	System.out.println("Working on i image");
        try {
            iImage.importDComplexData(0, realData, imagData, true, false);
            iImage.setOriginalExtents(iImage.getExtents());
        } catch (IOException e2) {
            // TODO Auto-generated catch block
            e2.printStackTrace();
        }
        
        System.out.println("IMported on i image");
        
        
        if(showInterImages) { 
        	this.iImage = iImage;
        	if(!inScript) {
                ViewJFrameImage iFrame = new ViewJFrameImage(iImage);
                iFrame.setVisible(true);
            } 
        } else {
        	ViewUserInterface.getReference().unRegisterImage(iImage);
        }
        	
        return iImage;
    }

    private ModelImage createBrainMask() {
        ModelImage brainMask = new ModelImage(ModelImage.BOOLEAN, new int[]{xDim,yDim,zDim}, "brainMask");
        JDialogBase.updateFileInfo(magImage, brainMask);
        
        for(int i=0; i<xDim; i++) {
            for(int j=0; j<yDim; j++) {
                for(int k=0; k<zDim; k++) {
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
        } else {
        	ViewUserInterface.getReference().unRegisterImage(brainMask);
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
