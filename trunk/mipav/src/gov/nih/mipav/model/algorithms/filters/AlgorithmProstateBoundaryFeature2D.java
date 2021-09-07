
package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Dimension;
import java.awt.Frame;
import java.io.*;
import java.util.*;

import javax.swing.JOptionPane;


/**
 * DOCUMENT ME!
 *
 *
 */
public class AlgorithmProstateBoundaryFeature2D extends AlgorithmBase implements AlgorithmInterface {
    
    /** DOCUMENT ME! */
    private ModelImage destImage = null;
    
    private ModelImage classificationImage = null;
 
	private ModelImage image; // source image
	
	private VOIVector VOIs;
	
	private boolean imageIntensityFilter;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	// Coherence Enhancing Diffusion
	private boolean coherenceEnhancingDiffusionFilter; 
    private ModelImage coherenceEnhancingDiffusionImage;
    private AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo;
	
    // Anisotropic Diffusion Filter
    private boolean regisotropicDiffusionFilter;
    private ModelImage regisotropicDiffusionImage;
    private AlgorithmRegularizedIsotropicDiffusion regisotropicDiffusionAlgo;
    
    // IHN3CorrectionImage
    private boolean IHN3CorrectionFilter;
    private ModelImage IHN3CorrectionImage;
    private AlgorithmIHN3Correction IHN3CorrectionAlgo;
    
    // Mode Algorithm
    private boolean modeFilter;
    private ModelImage modeImage;
    private AlgorithmMode modeAlgo;
    
    // Mean filter algorithm
    private boolean meanFilter;
    private ModelImage meanImage;
    private AlgorithmMean meanAlgo;
    
    // Median filter algorithm
    private boolean medianFilter;
    private ModelImage medianImage;
    private AlgorithmMedian medianAlgo;
    
    // Invert filter algorithm
    private boolean invertFilter;
    private ModelImage invertImage;
    private AlgorithmChangeType invertAlgo;
    
    // Haralick filter
    private boolean haralickFilter;
    private float[][][] haralickBuffer;
    private int totalHaralickFilters;
    
    // Gabor filter
    private boolean gaborFilter;
    private ModelImage gaborImage;
    private AlgorithmFrequencyFilter gaborAlgo;
    
    // gaussian blur
    private boolean gaussianFilter;
    private ModelImage gaussianImage;
    private AlgorithmGaussianBlur gaussianAlgo; 
    
    // gradient magnitude 
    private boolean gmFilter;
    private ModelImage gmImage;
    private AlgorithmGradientMagnitude gmAlgo;
    
    // hurst index
    private boolean hurstFilter;
    private ModelImage hurstImage;
    private AlgorithmHurstIndex hurstAlgo;
    
    // wavelet filter
    private boolean waveletFilter;
    private ModelImage[] waveletImage;
    private AlgorithmRiceWaveletTools waveletAlgo;
    
    
    private int numberFeatures;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmHaralickTexture object for black and white image.
     *
     */
    public AlgorithmProstateBoundaryFeature2D(ModelImage destImg, ModelImage classImage,  ModelImage srcImg, 
                                    boolean imageIntensityFilter,
                                    boolean coherenceEnhancingDiffusionFilter, 
                                    boolean regisotropicDiffusionFilter,
                                    boolean IHN3CorrectionFilter,
                                    boolean modeFilter,
                                    boolean meanFilter,
                                    boolean medianFilter,
                                    boolean invertFilter,
                                    boolean haralickFilter,
                                    boolean gaborFilter,
                                    boolean hurstFilter, 
                                    boolean waveletFilter, 
                                    boolean gaussianFilter,
                                    boolean gmFilter, 
                                    int numberFeatures) {
        super(null, srcImg);
        image = srcImage;
        
        destImage = destImg;
        classificationImage = classImage;
      
        this.imageIntensityFilter = imageIntensityFilter;
        if ( srcImage.getVOIs() != null ) {
        	this.VOIs = srcImage.getVOIs();
        }

        this.coherenceEnhancingDiffusionFilter = coherenceEnhancingDiffusionFilter;
        this.regisotropicDiffusionFilter = regisotropicDiffusionFilter;
        this.IHN3CorrectionFilter = IHN3CorrectionFilter;
        this.modeFilter = modeFilter;
        this.meanFilter = meanFilter;
        this.medianFilter = medianFilter;
        this.invertFilter = invertFilter;
        this.haralickFilter = haralickFilter;
        this.gaborFilter = gaborFilter;
        this.hurstFilter = hurstFilter;
        this.waveletFilter = waveletFilter;
        this.gaussianFilter = gaussianFilter;
        this.gmFilter = gmFilter;
         
        this.numberFeatures = numberFeatures;
        
    }
    
  

    // ~ Methods
	// --------------------------------------------------------------------------------------------------------

    /**
	 * Prepares this class for destruction.
	 */
    public void finalize() {
    	/*
    	if ( destImage != null ) {
    		destImage.disposeLocal();
    		destImage = null;
    	}
    	
    	if ( srcImage != null ) {
    		srcImage.disposeLocal();
    		srcImage = null;
    	}
        */
        
    	if (coherenceEnhancingDiffusionImage != null) {
        	coherenceEnhancingDiffusionImage.disposeLocal(); // clean up memory
        	coherenceEnhancingDiffusionImage = null;
        }
        
        if (regisotropicDiffusionImage != null) {
        	regisotropicDiffusionImage.disposeLocal(); // clean up memory
        	regisotropicDiffusionImage = null;
        }
        
        if ( IHN3CorrectionImage != null ) {
        	IHN3CorrectionImage.disposeLocal();
        	IHN3CorrectionImage = null;
        }     
        
        if ( modeImage != null ) {
        	modeImage.disposeLocal();
        	modeImage = null;
        }
        
        if ( meanImage != null ) {
        	meanImage.disposeLocal();
        	meanImage = null;
        }
        
        if ( medianImage != null ) {
            medianImage.disposeLocal();
            medianImage = null;
        }
        
        if ( invertImage != null ) {
        	invertImage.disposeLocal();
        	invertImage = null;
        }
        
        if ( gaborImage != null ) {
        	gaborImage.disposeLocal();
        	gaborImage = null;
        }
        
        if ( gaussianImage != null ) {
        	gaussianImage.disposeLocal();
        	gaussianImage = null;
        }
        
        if ( gmImage != null ) {
        	gmImage.disposeLocal();
        	gmImage = null;
        }
        
        if ( hurstImage != null ) {
        	hurstImage.disposeLocal();
        	hurstImage = null;
        }
        
        if ( haralickBuffer != null ) {
        	haralickBuffer = null;
        }
        
        /*
        if ( waveletImage != null ) {
        	for ( int i = 0; i < waveletImage.length; i++ ) {
        		waveletImage[i].disposeLocal();
        		waveletImage[i] = null;
        	}
        	waveletImage = null;
        }
        */        
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();
            return;
        }

        if (threadStopped) {
            finalize();
            return;
        }

        fireProgressStateChanged(0, null, "Running Prostate Feature Saves ...");
       
        if (coherenceEnhancingDiffusionFilter )  {
        	calculateCoherenceEnhancingDiffusion();
        }
        
        if ( regisotropicDiffusionFilter ) {
        	calculateRegIsotropicDiffusion();
        }
        
        if ( IHN3CorrectionFilter ) {
        	calculateIHN3Correction();
        }
        
        if ( modeFilter ) {
        	calculateMode();
        }
        
        if ( meanFilter ) {
        	calculateMean();
        }
        
        if ( medianFilter ) {
        	// calculateMedian();
        }
        
        if ( invertFilter ) {
        	calculateInvert();
        }
        
        if ( haralickFilter ) {
        	calculateHaralick();
        }
        
        if ( gaborFilter ) {
        	calculateGabor();
        }
        
        
        if ( hurstFilter ) {
        	calculateHurstIndex();
        }
        
        if ( waveletFilter ) {
        	calculateWavelet();
        }
        
        if ( gaussianFilter ) {
        	calculateGaussian();
        }
        
        if ( gmFilter ) {
        	calculateGM();
        }
        
        calculate();
        
    }
    
    
    private void calculateWavelet() {
    	int filterLength = 4;
        int numberOfLevels = 2; 
        boolean doWaveletImages = false; 
        int minimumLevel = 1; 
        int maximumLevel = 2;
        boolean redundant = true;
        
        try {
            waveletAlgo = new AlgorithmRiceWaveletTools(null, image, filterLength, redundant,
                              numberOfLevels, doWaveletImages, minimumLevel, maximumLevel, AlgorithmRiceWaveletTools.MINIMUM_PHASE);
            waveletAlgo.addListener(this);
            waveletAlgo.run();
            
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog WaveletThreshold: unable to allocate enough memory");
            return;
        }
    }
    
   private void calculateHurstIndex() {
    	
    	double minDistance = 1;    
    	double maxDistance = 7;
    	boolean integerDistanceRound = true;
    	
    	hurstImage = (ModelImage)image.clone();
    	hurstImage.setType(ModelStorageBase.FLOAT);
    	hurstImage.reallocate(ModelStorageBase.FLOAT);
    	try {
    		hurstAlgo = new AlgorithmHurstIndex(hurstImage, image, minDistance, maxDistance, integerDistanceRound);
    		hurstAlgo.run();
    	} catch (OutOfMemoryError e ) {
    		e.printStackTrace();
    	}
    	
    }
    
    
    private void calculateGM() {
    	try {

			float[] sigmas = new float[3];
			sigmas[0] = 1.0f;
			sigmas[1] = 1.0f;
			sigmas[2] = 1.0f;
			
			gmImage = (ModelImage) image.clone();
			gmImage.setType(ModelStorageBase.FLOAT);
			gmImage.reallocate(ModelStorageBase.FLOAT);

			gmAlgo = new AlgorithmGradientMagnitude(gmImage, image, sigmas, true, false);
			gmAlgo.addListener(this);

			gmAlgo.run();

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("gmFilter: unable to allocate enough memory");
			return;
		}
    }
    
    private void calculateGaussian() {

		try {

			float[] sigmas = new float[3];
			sigmas[0] = 1.0f;
			sigmas[1] = 1.0f;
			sigmas[2] = 1.0f;
			
			gaussianImage = (ModelImage) image.clone();
			gaussianImage.setType(ModelStorageBase.FLOAT);
			gaussianImage.reallocate(ModelStorageBase.FLOAT);

			gaussianAlgo = new AlgorithmGaussianBlur(gaussianImage, image, sigmas, true, true);
			gaussianAlgo.addListener(this);

			gaussianAlgo.run();

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("GaussianFilter: unable to allocate enough memory");
			return;
		}
    }
    
	private void calculateGabor() {

		try {

			float freqU = 0.0f;
			float freqV = 0.0f;
			float sigmaU = 0.1f;
			float sigmaV = 0.1f;
			float theta = 0.0f;

			gaborImage = (ModelImage) image.clone();
			// resultImage.setImageName(name);
			gaborImage.setType(ModelStorageBase.FLOAT);
			gaborImage.reallocate(ModelStorageBase.FLOAT);

			gaborAlgo = new AlgorithmFrequencyFilter(gaborImage, image, freqU,
					freqV, sigmaU, sigmaV, theta, false);
			gaborAlgo.addListener(this);

			gaborAlgo.run();

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("GaborFilter: unable to allocate enough memory");
			return;
		}
	}
    
    private void calculateInvert() {
    	try { 
    	invertImage = (ModelImage) image.clone();
    	invertImage.setType(ModelStorageBase.FLOAT);
		invertImage.reallocate(ModelStorageBase.FLOAT);
		
		double inTempMin = image.getMin();
		double inTempMax = image.getMax();
		double outTempMin = image.getMax();
		double outTempMax = image.getMin();
		
    	invertAlgo = new AlgorithmChangeType(invertImage, image, inTempMin, inTempMax, outTempMin,
                outTempMax, false);
    	
    	invertAlgo.addListener(this);
    	invertAlgo.run();
    	
    	} catch ( Exception e ) {
    		MipavUtil.displayError("invert calculation : unable to allocate enough memory");
    		return;
    	}
    	
    	
    	
    }
    
    /*
    private void calculateMedian() {
    	try {
    		int iters = 20;
    		int kernelSize = 3;
    		int kernelShape = 0;
    		float stdDev = 0f;
            boolean adaptiveSize = false;
            int maximumSize = 5;
    		boolean isProcessWholeImage = true;
    		
    		medianImage = (ModelImage) image.clone();
    		medianImage.setType(ModelStorageBase.FLOAT);
    		medianImage.reallocate(ModelStorageBase.FLOAT);
    		
    		medianAlgo = new AlgorithmMedian(medianImage, image, iters, kernelSize, kernelShape, stdDev,
                    adaptiveSize, maximumSize, isProcessWholeImage);
    		
    		medianAlgo.addListener(this);
    		medianAlgo.run();
    		
    		
    	} catch ( Exception e ) {
    		MipavUtil.displayError("mean calculation : unable to allocate enough memory");
    		return;
    	}
    }
    */
    
    private void calculateMean() {
    	try {
    		int kernelSize = 3;
    		boolean isProcessWholeImage = true;
    		
    		meanImage = (ModelImage) image.clone();
    		meanImage.setType(ModelStorageBase.FLOAT);
    		meanImage.reallocate(ModelStorageBase.FLOAT);
    		
    		meanAlgo = new AlgorithmMean(meanImage, image, kernelSize, isProcessWholeImage);
    		
    		meanAlgo.addListener(this);
    		meanAlgo.run();
    		
    		
    	} catch ( Exception e ) {
    		MipavUtil.displayError("mean calculation : unable to allocate enough memory");
    		return;
    	}
    }
    
    
    private void calculateMode() {
        
    	try {
    		int kernelSize = 3;
    		int kernelShape = 0;
    		boolean do25D = true;
    		boolean wholeImage = true;
    		
    		modeImage = (ModelImage) image.clone();
    		modeImage.setType(ModelStorageBase.FLOAT);
    		modeImage.reallocate(ModelStorageBase.FLOAT);
    		
    		modeAlgo = new AlgorithmMode(modeImage, image, kernelSize, kernelShape, do25D, wholeImage);
    		
    		modeAlgo.addListener(this);
    		modeAlgo.run();
    		
    		
    	} catch ( Exception e ) {
    		MipavUtil.displayError("mode calculation : unable to allocate enough memory");
    		return;
    	}
    }
    
	private void calculateRegIsotropicDiffusion() {
		try {
			
			int iters = 30;
			float stdDev = 1.0f;
			float contrast = 0.15f;
            boolean do25D = true;;
			
			regisotropicDiffusionImage = (ModelImage) image.clone();
			regisotropicDiffusionImage.setType(ModelStorageBase.FLOAT);
			regisotropicDiffusionImage.reallocate(ModelStorageBase.FLOAT);

			regisotropicDiffusionAlgo = new AlgorithmRegularizedIsotropicDiffusion(
					regisotropicDiffusionImage, image, iters, stdDev, 
					contrast, do25D);

			regisotropicDiffusionAlgo.addListener(this);

			regisotropicDiffusionAlgo.run();

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("RegularizedIsotropicDiffusion : unable to allocate enough memory");

			return;
		}
	}

	private void calculateCoherenceEnhancingDiffusion() {
		 float derivativeScale = 0.5f;
		 float diffusitivityDenom = 0.001f;
		 float gaussianScale = 2.0f;
		 int numIterations = 50;
		 boolean do25D = true;
		 boolean entireImage = true;
		
		try {
			coherenceEnhancingDiffusionImage = (ModelImage) image.clone();
			coherenceEnhancingDiffusionImage.setType(ModelStorageBase.FLOAT);
			coherenceEnhancingDiffusionImage.reallocate(ModelStorageBase.FLOAT);
			// resultImage.setImageName(name);
			// distanceImage.resetVOIs();

			// No need to make new image space because the user has choosen to
			// replace the source image
			// Make the algorithm class
			coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(
					coherenceEnhancingDiffusionImage, image, numIterations,
					diffusitivityDenom, derivativeScale, gaussianScale, do25D,
					entireImage);

		
			coherenceEnhancingDiffusionAlgo.addListener(this);

			coherenceEnhancingDiffusionAlgo.run();

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("calculateCoherenceEnhancingDiffusion : unable to allocate enough memory");

			return;
		}
	}
    
	public void calculateIHN3Correction() {
		try {
			ModelImage fieldImage = null;
			float threshold = 1.0f;
			int maxIter = 50;
			float endTol = 0.001f;
			float fieldDistance = 16.588f;
			float shrink = 4.0f;
			float kernelfwhm = 0.15f;
			float noise = 0.01f;;
			boolean regionFlag = true;
			boolean autoThreshold = false;
			boolean useScript = false;
			
			IHN3CorrectionImage = (ModelImage) image.clone();
			IHN3CorrectionImage.setType(ModelStorageBase.FLOAT);
			IHN3CorrectionImage.reallocate(ModelStorageBase.FLOAT);
			
			// Make algorithm
			IHN3CorrectionAlgo = new AlgorithmIHN3Correction(IHN3CorrectionImage, fieldImage, image, threshold, maxIter, endTol,
                                                 fieldDistance, shrink, kernelfwhm, noise, regionFlag, autoThreshold,
                                                 useScript);
		
			IHN3CorrectionAlgo.addListener(this);

			IHN3CorrectionAlgo.run();
			
		
		} catch ( Exception e ) {
			MipavUtil.displayError("calculateIHN3Correction : unable to allocate enough memory");

			return;
		}
		
		
	}
	
	
    /**
     * DOCUMENT ME!
     */
    private void calculate() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
      
        srcImage.calcMinMax();
     
        int windowSize = 7;
        int halfWin = (windowSize - 1) / 2;
        int xStart = halfWin;
        int xEnd = xDim - 1 - halfWin;
        int yStart = halfWin;
        int yEnd = yDim - 1 - halfWin;
       
        int x, y;
        int i, j;
        
        float[][] resultBuffer = null;
        float[][] resultBufferClass = null;
        int currentResult = 0;
        int pos;
     
        int z; 
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }
        
        System.err.println("numberFilters = " + numberFeatures);
      
        resultBuffer = new float[numberFeatures][sliceSize];
        resultBufferClass = new float[numberFeatures][sliceSize];


		for (z = 0; z < zDim; z++) {
			try {

				float[] intensityBuffer = new float[sliceSize];
				float[] coherenceEnhancingDiffusionBuffer = new float[sliceSize];
				float[] regisotropicDiffusionBuffer = new float[sliceSize];
				float[] IHN3CorrectionBuffer = new float[sliceSize];
				float[] modeBuffer = new float[sliceSize];
				float[] meanBuffer = new float[sliceSize];
				float[] medianBuffer = new float[sliceSize];
				float[] invertBuffer = new float[sliceSize];
				float[] gaborBuffer = new float[sliceSize];
				float[] hurstBuffer = new float[sliceSize];
				float[][] waveletBuffer = new float[4][sliceSize];
				float[] gaussianBuffer = new float[sliceSize];
				float[] gmBuffer = new float[sliceSize];
				
				if ( imageIntensityFilter ) {
					srcImage.exportData(z * sliceSize, sliceSize, intensityBuffer);
				}
				
				if (coherenceEnhancingDiffusionFilter) {
					coherenceEnhancingDiffusionImage.exportData(z * sliceSize,
							sliceSize, coherenceEnhancingDiffusionBuffer);
				}

				if (regisotropicDiffusionFilter) {
					regisotropicDiffusionImage.exportData(z * sliceSize,
							sliceSize, regisotropicDiffusionBuffer);
				}
				
				
				if ( IHN3CorrectionFilter ) {
					IHN3CorrectionImage.exportData(z * sliceSize,
							sliceSize, IHN3CorrectionBuffer);
				}
				
				if ( modeFilter ) {
					modeImage.exportData(z * sliceSize,
							sliceSize, modeBuffer);
				}
					
				if ( meanFilter ) {
					meanImage.exportData(z * sliceSize,
							sliceSize, meanBuffer);
				}
				
				if ( medianFilter ) {
					medianImage.exportData(z * sliceSize,
							sliceSize, medianBuffer);
				}

				if ( invertFilter ) {
					invertImage.exportData(z * sliceSize,
							sliceSize, invertBuffer);
				}
				
				if ( gaborFilter ) {
					gaborImage.exportData(z * sliceSize,
							sliceSize, gaborBuffer);
				}
				
				if ( hurstFilter ) {
					hurstImage.exportData(z * sliceSize,
							sliceSize, hurstBuffer);
				}
				
				if ( waveletFilter ){
					for (int k = 0; k < 1; k++ ) {
						waveletImage[k].exportData(z*sliceSize, sliceSize, waveletBuffer[k]);
					}
				}
				
				if ( gaussianFilter ) {
					gaussianImage.exportData(z * sliceSize,
							sliceSize, gaussianBuffer);
				}
				
				if ( gmFilter ) {
					gmImage.exportData(z * sliceSize,
							sliceSize, gmBuffer);
				}
				
				for (i = 0; i < numberFeatures; i++) {
					for (j = 0; j < sliceSize; j++) {
						resultBuffer[i][j] = 0;
						resultBufferClass[i][j] = 0;
					}
				}

				for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

					fireProgressStateChanged(
							((int) ((z * 100.0f / zDim) + ((y - yStart) * (100.0f / (zDim * (yEnd - yStart)))))),
							null, null);

					for (x = xStart; x <= xEnd; x++) {
						pos = x + (y * xDim);
						currentResult = 0;
						
						if ( imageIntensityFilter ) {
							double cedValue = intensityBuffer[pos];
							resultBuffer[currentResult][pos] = (float)cedValue;
							currentResult++;
						}
						
						if (coherenceEnhancingDiffusionFilter) {
							float cedValue = coherenceEnhancingDiffusionBuffer[pos];
							resultBuffer[currentResult][pos] = cedValue;
							currentResult++;
						}

						if (regisotropicDiffusionFilter) {
							float adValue = regisotropicDiffusionBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						if ( IHN3CorrectionFilter ) {
							float adValue = IHN3CorrectionBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}

						if ( modeFilter ) {
							float adValue = modeBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						if ( meanFilter ) {
							float adValue = meanBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						if ( medianFilter ) {
							float adValue = medianBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						if ( invertFilter ) {
							float adValue = invertBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						if ( gaborFilter ) {
							float adValue = gaborBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						
						if ( gaussianFilter ) {
							float adValue = gaussianBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						if ( gmFilter ) {
							float adValue = gmBuffer[pos];
							resultBuffer[currentResult][pos] = adValue;
							currentResult++;
						}
						
						// haralick
						if ( haralickFilter ) {
						     int k = 0;
						     resultBuffer[currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     /*
						     resultBuffer[currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     resultBuffer[z][currentResult][pos] = haralickBuffer[z][k][pos];
						     currentResult++; k++;
						     */
						}
						
						if ( hurstFilter ) {
							resultBuffer[currentResult][pos] = hurstBuffer[pos];
							currentResult++;
						}
						
						if ( waveletFilter ) {
							int k = 0;
							resultBuffer[currentResult][pos] = waveletBuffer[k][pos];
							currentResult++; k++;
							/*
							resultBuffer[z][currentResult][pos] = waveletBuffer[k][pos];
							currentResult++; k++;
							resultBuffer[z][currentResult][pos] = waveletBuffer[k][pos];
							currentResult++; k++;
							resultBuffer[z][currentResult][pos] = waveletBuffer[k][pos];
							currentResult++; k++;
							*/
						}
						
						if (VOIs.size() > 0) {
							
							Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);

							if (vArray != null) {
								if (vArray[z] != null && vArray[z].size() > 0) {
									for (int w = 0; w < vArray[z].size(); w++) {
										VOIBase v = vArray[z].get(w);
										if (v instanceof VOIContour) {
											if (((VOIContour) v).contain(x, y)) {
												for (int q = 0; q < currentResult; q++) {
													resultBufferClass[q][pos] = 1;
												}

											} else {
												for (int q = 0; q < currentResult; q++) {
													if (resultBufferClass[q][pos] != 1)
														resultBufferClass[q][pos] = -1;
												}
											}
										}
									}
								}

							}
						}
						
					} // for (x = xStart; x <= xEnd; x++)
				} // for (y = yStart; y <= yEnd && !threadStopped; y++)
				
				if (threadStopped) {
					finalize();
					return;
				}

				for (i = 0; i < numberFeatures; i++) {

					try {

						destImage.importData((z * numberFeatures * sliceSize) + i * sliceSize,
								resultBuffer[i], false);

						if (VOIs.size() > 0) {
							classificationImage.importData((z * numberFeatures * sliceSize) + i * sliceSize,
									resultBufferClass[i], false);
						}
					} catch (IOException error) {
						MipavUtil
								.displayError("image, AlgorithmProstateFeature: IOException on destImage["
										+ i
										+ "].importData(0,resultBuffer["
										+ i + "],false)");
						setCompleted(false);

						return;
					}
				} // for (i = 0; i < resultNumber; i++)
				
				
			} catch (IOException error) {
				MipavUtil
						.displayError("AlgorithmHaralickTexture: IOException on srcImage.exportData(0,sliceSize,sourceBuffer)");
				setCompleted(false);

				return;
			}
			
		
			
		} // for (z = 0; z < zDim; z++)

       
        
		
		destImage.calcMinMax();
		classificationImage.calcMinMax();
		
		setCompleted(true);

		return;
	}

 private void calculateHaralick() {
    	
		int windowSize = 7;
		int offsetDistance = 1;
		int greyLevels = 32;
		boolean ns = false;
		boolean nesw = false;
		boolean ew = false;
		boolean senw = false;
		boolean invariantDir = true;
		boolean contrast = false;
		boolean dissimilarity = false;
		boolean homogeneity = false;
		boolean inverseOrder1 = false;
		boolean asm = false;
		boolean energy = false;
		boolean maxProbability = false;
		boolean entropy = false;
		boolean mean = true;
		boolean variance = true;
		boolean standardDeviation = true;
		boolean correlation = true;
		boolean shade = false;
		boolean promenance = false;
    	
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
        boolean doneNS = false;
        boolean doneNESW = false;
        boolean doneEW = false;
        boolean doneSENW = false;
        boolean doneInvariant = false;
        int iDir;
        double imageMin;
        double imageMax;
        srcImage.calcMinMax();
        imageMin = srcImage.getMin();
        imageMax = srcImage.getMax();

        int numDirections = 0;
        int numOperators = 0;
        double[] sourceBuffer = new double[sliceSize];
        byte[]  byteBuffer = new byte[sliceSize];
        float[] floatBuffer;
        int[][] nsBuffer = null;
        int[][] neswBuffer = null;
        int[][] ewBuffer = null;
        int[][] senwBuffer = null;
        int[][] invariantDirBuffer = null;

        // gray level co-occurrence matrix
        float[][] glcm = null;
        int matrixSize;
        int halfWin = (windowSize - 1) / 2;
        int xStart = halfWin;
        int xEnd = xDim - 1 - halfWin;
        int yStart = halfWin;
        int yEnd = yDim - 1 - halfWin;
        int numValues = (xEnd - xStart + 1) * (yEnd - yStart + 1);
        int x, y;
        int i, j;
        int index;
        int resultNumber;
        float matrixSum;
        
        int currentResult = 0;
        int pos;
        float glcmMean = 0.0f;
        float glcmVariance = 0.0f;
        float glcmASM = 0.0f;
        boolean skip;
        boolean rescale = false;
        double range = imageMax - imageMin;
        double factor = (greyLevels - 1)/range;
        float sum;
        float product;
        int z;
        double total;
        double totalSquared;
        double value;
        double zMean;
        double zStdDev;
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }
        
        if ((srcImage.getType() == ModelStorageBase.FLOAT) || (srcImage.getType() == ModelStorageBase.DOUBLE) ||
            (srcImage.getType() == ModelStorageBase.ARGB_FLOAT)) {
            rescale = true;
        } else if (range >= 64) {
            rescale = true;
        }
        
        if (ns) {
            numDirections++;
        }

        if (nesw) {
            numDirections++;
        }

        if (ew) {
            numDirections++;
        }

        if (senw) {
            numDirections++;
        }

        if (invariantDir) {
            numDirections++;
        }

        if (contrast) {
            numOperators++;
        }

        if (dissimilarity) {
            numOperators++;
        }

        if (homogeneity) {
            numOperators++;
        }

        if (inverseOrder1) {
            numOperators++;
        }

        if (asm) {
            numOperators++;
        }

        if (energy) {
            numOperators++;
        }

        if (maxProbability) {
            numOperators++;
        }

        if (entropy) {
            numOperators++;
        }

        if (mean) {
            numOperators++;
        }

        if (variance) {
            numOperators++;
        }

        if (standardDeviation) {
            numOperators++;
        }

        if (correlation) {
            numOperators++;
        }
        
        if (shade) {
            numOperators++;
        }
        
        if (promenance) {
            numOperators++;
        }

        resultNumber = numDirections * numOperators;

        if (rescale) {
            matrixSize = greyLevels;
        }
        else {
            matrixSize = (int)Math.round(range) + 1;
        }

        if (ns || invariantDir) {
            nsBuffer = new int[matrixSize][matrixSize];
        }

        if (nesw || invariantDir) {
            neswBuffer = new int[matrixSize][matrixSize];
        }

        if (ew || invariantDir) {
            ewBuffer = new int[matrixSize][matrixSize];
        }

        if (senw || invariantDir) {
            senwBuffer = new int[matrixSize][matrixSize];
        }

        if (invariantDir) {
            invariantDirBuffer = new int[matrixSize][matrixSize];
        }

        glcm = new float[matrixSize][matrixSize];
        haralickBuffer = new float[zDim][resultNumber][sliceSize];


		for (z = 0; z < zDim; z++) {
			try {

				srcImage.exportData(z * sliceSize, sliceSize, sourceBuffer);

			} catch (IOException error) {
				MipavUtil
						.displayError("AlgorithmHaralickTexture: IOException on srcImage.exportData(0,sliceSize,sourceBuffer)");
				setCompleted(false);

				return;
			}

			for (i = 0; i < sliceSize; i++) {
				sourceBuffer[i] -= imageMin;
			}

			if (rescale) {
				for (i = 0; i < sliceSize; i++) {
					byteBuffer[i] = (byte) ((sourceBuffer[i] * factor) + 0.5f);
				}
			} else {
				for (i = 0; i < sliceSize; i++) {
					byteBuffer[i] = (byte) Math.round(sourceBuffer[i]);
				}
			}

			for (i = 0; i < resultNumber; i++) {
				for (j = 0; j < sliceSize; j++) {
					haralickBuffer[z][i][j] = 0;
				}
			}

			for (y = yStart; (y <= yEnd) && !threadStopped; y++) {

				fireProgressStateChanged(
						((int) ((z * 100.0f / zDim) + ((y - yStart) * (100.0f / (zDim * (yEnd - yStart)))))),
						null, null);

				for (x = xStart; x <= xEnd; x++) {
					pos = x + (y * xDim);
					doneNS = false;
					doneNESW = false;
					doneEW = false;
					doneSENW = false;
					doneInvariant = false;
					currentResult = 0;

					for (iDir = 0; iDir < numDirections; iDir++) {
						skip = false;

						if ((ns || invariantDir) && (!doneNS)) {
							doneNS = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									nsBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

								for (i = x - halfWin; i <= (x + halfWin); i++) {
									index = i + (j * xDim);
									nsBuffer[byteBuffer[index]][byteBuffer[index
											+ xDim]]++;
									nsBuffer[byteBuffer[index + xDim]][byteBuffer[index]]++;
								}
							}

							if (ns) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* windowSize;

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = nsBuffer[i][j] / matrixSum;
									}
								}

								skip = true;
							} // if (ns)
						}

						if ((nesw || invariantDir) && (!doneNESW) && (!skip)) {
							doneNESW = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									neswBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

								for (i = x - halfWin + offsetDistance; i <= (x + halfWin); i++) {
									index = i + (j * xDim);
									neswBuffer[byteBuffer[index]][byteBuffer[index
											+ xDim - 1]]++;
									neswBuffer[byteBuffer[index + xDim - 1]][byteBuffer[index]]++;
								}
							}

							if (nesw) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* (windowSize - offsetDistance);

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = neswBuffer[i][j]
												/ matrixSum;
									}
								}

								skip = true;
							} // if (nesw)
						}

						if ((ew || invariantDir) && (!doneEW) && (!skip)) {
							doneEW = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									ewBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin); j++) {

								for (i = x - halfWin; i <= (x + halfWin - offsetDistance); i++) {
									index = i + (j * xDim);
									ewBuffer[byteBuffer[index]][byteBuffer[index + 1]]++;
									ewBuffer[byteBuffer[index + 1]][byteBuffer[index]]++;
								}
							}

							if (ew) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* windowSize;

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = ewBuffer[i][j] / matrixSum;
									}
								}

								skip = true;
							} // if (ew)
						}

						if ((senw || invariantDir) && (!doneSENW) && (!skip)) {
							doneSENW = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									senwBuffer[i][j] = 0;
								}
							}

							for (j = y - halfWin; j <= (y + halfWin - offsetDistance); j++) {

								for (i = x - halfWin; i <= (x + halfWin - offsetDistance); i++) {
									index = i + (j * xDim);
									senwBuffer[byteBuffer[index]][byteBuffer[index
											+ xDim + 1]]++;
									senwBuffer[byteBuffer[index + xDim + 1]][byteBuffer[index]]++;
								}
							}

							if (senw) {
								matrixSum = 2.0f
										* (windowSize - offsetDistance)
										* (windowSize - offsetDistance);

								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										glcm[i][j] = senwBuffer[i][j]
												/ matrixSum;
									}
								}

								skip = true;
							} // if (senw)
						}

						if (invariantDir && (!doneInvariant) && (!skip)) {
							doneInvariant = true;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									invariantDirBuffer[i][j] = nsBuffer[i][j]
											+ neswBuffer[i][j] + ewBuffer[i][j]
											+ senwBuffer[i][j];
								}
							}

							matrixSum = (4.0f * (windowSize - offsetDistance) * (windowSize - offsetDistance))
									+ (4.0f * (windowSize - offsetDistance) * windowSize);

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcm[i][j] = invariantDirBuffer[i][j]
											/ matrixSum;
								}
							}
						} // else if (invariantDir && (!doneInvariant)

						if (contrast) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									haralickBuffer[z][currentResult][pos] += (i - j)
											* (i - j) * glcm[i][j];
								}
							}

							currentResult++;
						} // if (contrast)

						if (dissimilarity) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									haralickBuffer[z][currentResult][pos] += Math
											.abs(i - j) * glcm[i][j];
								}
							}

							currentResult++;
						} // if (dissimilarity)

						if (homogeneity) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									haralickBuffer[z][currentResult][pos] += glcm[i][j]
											/ (1.0f + ((i - j) * (i - j)));
								}
							}

							currentResult++;
						} // if (homogeneity)

						if (inverseOrder1) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									haralickBuffer[z][currentResult][pos] += glcm[i][j]
											/ (1.0f + Math.abs(i - j));
								}
							}

							currentResult++;
						} // if (inverseOrder1)

						if (asm || energy) {
							glcmASM = 0.0f;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcmASM += glcm[i][j] * glcm[i][j];
								}
							}

							if (!energy) {
								haralickBuffer[z][currentResult][pos] = glcmASM;
								currentResult++;
							} else if (!asm) {
								haralickBuffer[z][currentResult][pos] = (float) Math
										.sqrt(glcmASM);
								currentResult++;
							} else {
								haralickBuffer[z][currentResult][pos] = glcmASM;
								currentResult++;
								haralickBuffer[z][currentResult][pos] = (float) Math
										.sqrt(glcmASM);
								currentResult++;
							}
						} // if (asm || energy)

						if (maxProbability) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {

									if (glcm[i][j] > haralickBuffer[z][currentResult][pos]) {
										haralickBuffer[z][currentResult][pos] = glcm[i][j];
									}
								}
							}

							currentResult++;
						} // if (maxProbability)

						if (entropy) {

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {

									if (glcm[i][j] > 0.0f) {
										haralickBuffer[z][currentResult][pos] -= glcm[i][j]
												* Math.log(glcm[i][j]);
									}
								}
							}

							currentResult++;
						} // if (entropy)

						if (mean || variance || standardDeviation
								|| correlation || shade || promenance) {
							glcmMean = 0.0f;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcmMean += i * glcm[i][j];
								}
							}

							if (mean) {
								haralickBuffer[z][currentResult][pos] = glcmMean;
								currentResult++;
							}
						} // if (mean || variance || standardDeviation ||
							// correlation)

						if (variance || standardDeviation || correlation) {
							glcmVariance = 0.0f;

							for (i = 0; i < matrixSize; i++) {

								for (j = 0; j < matrixSize; j++) {
									glcmVariance += glcm[i][j] * (i - glcmMean)
											* (i - glcmMean);
								}
							}

							if ((!standardDeviation) && variance) {
								haralickBuffer[z][currentResult][pos] = glcmVariance;
								currentResult++;
							} else if ((!variance) && standardDeviation) {
								haralickBuffer[z][currentResult][pos] = (float) Math
										.sqrt(glcmVariance);
								currentResult++;
							} else if (variance && standardDeviation) {
								haralickBuffer[z][currentResult][pos] = glcmVariance;
								currentResult++;
								haralickBuffer[z][currentResult][pos] = (float) Math
										.sqrt(glcmVariance);
								currentResult++;
							}
						} // if (variance || standardDeviation || correlation)

						if (correlation) {

							if (glcmVariance != 0) {
								for (i = 0; i < matrixSize; i++) {

									for (j = 0; j < matrixSize; j++) {
										haralickBuffer[z][currentResult][pos] += glcm[i][j]
												* (i - glcmMean)
												* (j - glcmMean) / glcmVariance;
									}
								}
							} // if (glcmVariance != 0)
							else {
								// When an image area is completely uniform, the
								// GLCM variance is zero, just as the
								// first-order
								// image variance is zero. As a result, the
								// denominator of the correlation equation
								// becomes 0,
								// and the correlation becomes undefined. The
								// undefined value is set to 1, as the
								// correlation
								// among the original pixel values is perfect
								haralickBuffer[z][currentResult][pos] = 1.0f;
							}

							currentResult++;
						} // if (correlation)

						if (shade) {
							for (i = 0; i < matrixSize; i++) {
								for (j = 0; j < matrixSize; j++) {
									sum = (i + j - 2 * glcmMean);
									haralickBuffer[z][currentResult][pos] += glcm[i][j]
											* sum * sum * sum;
								}
							}
							currentResult++;
						} // if (shade)

						if (promenance) {
							for (i = 0; i < matrixSize; i++) {
								for (j = 0; j < matrixSize; j++) {
									sum = (i + j - 2 * glcmMean);
									product = sum * sum;
									haralickBuffer[z][currentResult][pos] += glcm[i][j]
											* product * product;
								}
							}
							currentResult++;
						} // if (promenance)
					} // for (iDir = 0; iDir < numDirections; iDir++)
				} // for (x = xStart; x <= xEnd; x++)
			} // for (y = yStart; y <= yEnd && !threadStopped; y++)

			
			if (threadStopped) {
				finalize();

				return;
			}

		} // for (z = 0; z < zDim; z++)
      
     
        return;
    }
    
    
    public int whichClass(int pos, int centerX, int centerY, int xDim, int yDim ) {
    	
    	int xPos = pos % xDim;
		int yPos = pos / xDim;
    	
		int classLocation;
		
		if ( ( xPos >= 0 && xPos < centerX) && ( yPos >= 0 && yPos < centerY ) ) {
			classLocation = 2;
		} else if ( (xPos > centerX &&  xPos <= xDim ) && ( yPos >= 0 && yPos < centerY ) ) {
			classLocation = 3;
		} else if ( ( xPos >= 0 && xPos < centerX ) && ( yPos > centerY && yPos <= yDim ) ) {
			classLocation = 4;
		} else {
			classLocation = 5;
		}
		
		return classLocation;
    	
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	
    	if (algorithm instanceof AlgorithmCoherenceEnhancingDiffusion) {

            if ((algorithm.isCompleted() == true) && (coherenceEnhancingDiffusionImage != null) && ( coherenceEnhancingDiffusionFilter )) {
               
                try {
                	coherenceEnhancingDiffusionImage = coherenceEnhancingDiffusionAlgo.getDestImage();
                 
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("AlgorithmCoherenceEnhancingDiffusion complete, Out of memory: unable to open new frame");
                }
            } 
            coherenceEnhancingDiffusionAlgo.finalize();
            coherenceEnhancingDiffusionAlgo = null;
        }

        if (algorithm instanceof AlgorithmRiceWaveletTools) {
            if (waveletAlgo.isCompleted()) {
                 waveletImage = waveletAlgo.getWaveletImages();
                 /*
                 if (waveletImage != null) {
                     for (int i = 0; i < waveletImage.length; i++) {
                         
                         if (waveletImage[i] != null) {

                             // waveletImage is same size as original image
                             // updateFileInfo(image, waveletImage[i]);
     
                             try {
                                 new ViewJFrameImage(waveletImage[i]);
                             } catch (OutOfMemoryError error) {
                                 MipavUtil.displayError("Out of memory: Unable to open wavelet image frame");
                             }
                         } // if (waveletImage[i] != null)
                         else {
                             MipavUtil.displayError("waveletImage[" + i + "] is null");
                         }
                     } // for (i = 0; i < waveletImage.length; i++)
                 } else {
                     MipavUtil.displayError("waveletImage array is null");
                 }
                 */
                 waveletAlgo.setCompleted(true);
                 waveletAlgo.finalize();
                 waveletAlgo = null;
            }
        }
        
        if (algorithm instanceof AlgorithmRegularizedIsotropicDiffusion) {

            if ((algorithm.isCompleted() == true) && (regisotropicDiffusionImage != null) && ( regisotropicDiffusionFilter )) {
               
                try {
                	regisotropicDiffusionImage = regisotropicDiffusionAlgo.getDestImage();
                 
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("AlgorithmRegularizedIsotropicDiffusion, Out of memory: unable to open new frame");
                }
            }
            regisotropicDiffusionAlgo.finalize();
            regisotropicDiffusionAlgo = null;
        }

    	if ( algorithm instanceof AlgorithmIHN3Correction ) {
    		if ( algorithm.isCompleted() == true  && ( IHN3CorrectionImage != null) && ( IHN3CorrectionFilter )) {
    			try {
    				IHN3CorrectionImage = IHN3CorrectionAlgo.getDestImage();
    			} catch ( Exception e ) {
    				System.gc();
                    MipavUtil.displayError("AlgorithmIHN3Correction, Out of memory: unable to open new frame");
    			}
    			IHN3CorrectionAlgo.finalize();
    			IHN3CorrectionAlgo = null;
    		}
    	}
    	
    	if ( algorithm instanceof AlgorithmMode ) {
    		if ( algorithm.isCompleted() == true  && ( modeImage != null) && ( modeFilter )) {
    			try {
    				modeImage = modeAlgo.getDestImage();
    			} catch ( Exception e ) {
    				System.gc();
                    MipavUtil.displayError("AlgorithmIHN3Correction, Out of memory: unable to open new frame");
    			}
    			modeAlgo.finalize();
    			modeAlgo = null;
    		}
    	}
    	
    	if ( algorithm instanceof AlgorithmMean ) {
    		if ( algorithm.isCompleted() == true  && ( meanImage != null) && ( meanFilter )) {
    			try {
    				meanImage = meanAlgo.getDestImage();
    			} catch ( Exception e ) {
    				System.gc();
                    MipavUtil.displayError("AlgorithmMean, Out of memory: unable to open new frame");
    			}
    			meanAlgo.finalize();
    			meanAlgo = null;
    		}
    	}
    	
    	if ( algorithm instanceof AlgorithmMedian ) {
    		if ( algorithm.isCompleted() == true  && ( medianImage != null) && ( medianFilter )) {
    			try {
    				medianImage = medianAlgo.getDestImage();
    			} catch ( Exception e ) {
    				System.gc();
                    MipavUtil.displayError("AlgorithmMean, Out of memory: unable to open new frame");
    			}
    			medianAlgo.finalize();
    			medianAlgo = null;
    		}
    	}
    	
    	if ( algorithm instanceof AlgorithmChangeType ) {
    		if ( algorithm.isCompleted() == true  && ( invertImage != null) && ( invertFilter )) {
    			try {
    				invertImage = invertAlgo.getDestImage();
    			} catch ( Exception e ) {
    				System.gc();
                    MipavUtil.displayError("AlgorithmMean, Out of memory: unable to open new frame");
    			}
    			invertAlgo.finalize();
    			invertAlgo = null;
    		}
    	}
    	
	}

   
    
}
