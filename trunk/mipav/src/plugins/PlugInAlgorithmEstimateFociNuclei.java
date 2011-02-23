import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Dimension;
import java.io.*;

//import java.util.*;

/**
 * @author joshim2
 * This is a plugin which estimates the foci over nuclei in an image. 
 */

public class PlugInAlgorithmEstimateFociNuclei extends AlgorithmBase {
	
	// ~ Instance fields ---------------------------------------------------------------------------------------
	
	/** Red channel grayscale image */
	private ModelImage grayRedImage;
	
	/** Green channel grayscale image */
	private ModelImage grayGreenImage;
	
	/** Blue channel grayscale image */
	private ModelImage grayBlueImage;
	
	/** Segmentation result image */
	private ModelImage segResultImage;
	
	/** Particle analysis result image */
	private ModelImage particleAnalysisResultImage;
	
	/** ID Objects Result Image */
	private ModelImage idObjectsResultImage;
	
	/** Result Image */
	private ModelImage resultImage;
	
	/** Segmentation: Number of classes */
	private int nClasses;
	
	/** Segmentation: Desired exponent value */
	private float expValue;
	
	/** Segmentation: End tolerance */
	private float endTol;
	
	/** Segmentation: Max. number of iterations */
	private int maxIter;
	
	/** Segmentation: Signal threshold */
	private float threshold;
	
	/** Particle Analysis: Number of open */
    private int itersOpen;
    
    /** Particle Analysis: Kernel Size (Open) */
    private float kernelSizeOpen;
    
    /** Particle Analysis: Kernel Type (Open) */
    private int kernelOpen;
    
    /** Particle Analysis: Number of close */
    private int itersClose;
    
    /** Particle Analysis: Kernel Size (Close) */
    private float kernelSizeClose;
    
    /** Particle Analysis: Kernel Type (Close) */
    private int kernelClose;
    
    /** ID Objects: Max size */
    private int maxSize;
    
    /** ID Objects: Min size */
    private int minSize;
    
    //	~ Constructors -----------------------------------------------------------------------------------------

    /**
     * Constructor for result images in which changes are placed in a predetermined destination image.
     *
     * @param  srcImg   Source image model.
     */
    public PlugInAlgorithmEstimateFociNuclei(ModelImage srcImg, int SegNClasses, float SegExpValue, float SegEndTol, 
    											int SegMaxIter, float SegThreshold, int paItersOpen,
    											float paKernelSizeOpen, int paKernelOpen, int paItersClose,
    											float paKernelSizeClose, int paKernelClose, int idMaxSize,
    											int idMinSize) {
        super(null, srcImg);
    	nClasses = SegNClasses;
        expValue = SegExpValue;
        endTol = SegEndTol;
        maxIter = SegMaxIter;
        threshold = SegThreshold;
        itersOpen = paItersOpen;
        kernelSizeOpen = paKernelSizeOpen;
        kernelOpen = paKernelOpen;
        itersClose = paItersClose;
        kernelSizeClose = paKernelSizeClose;
        kernelClose = paKernelClose;
        maxSize = idMaxSize;
        minSize = idMinSize;
        
        
    }
    
    //  ~ Methods ----------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
        particleAnalysisResultImage.disposeLocal();
        idObjectsResultImage.disposeLocal();
               
    }
    
    /** 
     * Starts the algorithm'
     */  
     
    public void runAlgorithm() {
    	
    	if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.getNDims() == 2) {
        		calcStoreInDest2D();
        	}
        	else {
        		displayError("This plugin supports only 2D images");
        	}
        
    }
    
    /**
     * This method converts the RGB image into 3 grayscale images (one for each channel). The blue channel image
     * (nuclei) is then segmented into 2 distinct classes. The segmented image is then subjected to particle
     * analysis algorithm to classify distinct objects. Finally, the number of foci contained in each nuclei is 
     * estimated. 
     */
    
    public void calcStoreInDest2D() {
    	
    	// Progressbar ratios
    	float[] ratios = new float[3];
    	ratios[0] = (float) 0.05;
    	ratios[1] = (float) 0.4;
    	ratios[2] = (float) 0.5;
    	
    	// Convert source RGB image to grays
    	if (!srcImage.isColorImage()) {
    		displayError("The source image must be an RGB image");
     	} else {
     		
     		/* First save each of the red, green, and blue channels in a
             * separate ModelImage for reconstruction. */
            /* Determine the type of the color image: */
            int iType = srcImage.getType();

            if (iType == ModelStorageBase.ARGB) {
                iType = ModelStorageBase.UBYTE;
            }

            if (iType == ModelStorageBase.ARGB_FLOAT) {
                iType = ModelStorageBase.FLOAT;
            }

            if (iType == ModelStorageBase.ARGB_USHORT) {
                iType = ModelStorageBase.USHORT;
            }

            /* Create three separate ModelImages of the same type: */
            grayRedImage = new ModelImage(iType, srcImage.getExtents(), srcImage.getImageName() + "_Red");
            grayGreenImage = new ModelImage(iType, srcImage.getExtents(), srcImage.getImageName() + "_Green");
            grayBlueImage = new ModelImage(iType, srcImage.getExtents(), srcImage.getImageName() + "_Blue");
            
            fireProgressStateChanged(0, null, "Converting RGB to grays ...");
     		
     		AlgorithmRGBtoGrays algoRGBtoGrays = new AlgorithmRGBtoGrays(grayRedImage, grayGreenImage,
     													grayBlueImage, srcImage);
     		
     		linkProgressToAlgorithm(algoRGBtoGrays);
            algoRGBtoGrays.setProgressValues(generateProgressValues(0, 5));
     		/* Must not run in separate thread, since we need the results before
             * proceeding to the next step: */
     		algoRGBtoGrays.setRunningInSeparateThread(false);
     		algoRGBtoGrays.run();
     		algoRGBtoGrays.finalize();
     		algoRGBtoGrays = null;
     		
     		// Display RGB images
     		try {
                new ViewJFrameImage(grayRedImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
     		
            try {
                new ViewJFrameImage(grayGreenImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
            
            try {
                new ViewJFrameImage(grayBlueImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
            
     		fireProgressStateChanged(5);
     	}
    	
    	// Segmentation
    	if (grayBlueImage == null) {
    		displayError("The source blue channel is empty");
    	} else {
    		segResultImage = segmentation(grayBlueImage);
    	}
    	
    	// Particle analysis
    	if (segResultImage == null) {
    		displayError("Segmented image is null");
    	} else {
    		particleAnalysisResultImage = (ModelImage) segResultImage.clone();
    		particleAnalysis(particleAnalysisResultImage);
    	}
    	
    	// Find the number of foci in the selected nuclei
    	if (particleAnalysisResultImage == null) {
    		displayError("Particle Analysis result is an null image");
    	} else {
    		findFociInNuclei(grayRedImage, particleAnalysisResultImage);
    		
    	}
    	
    	
    }
    
    private ModelImage segmentation(ModelImage blueImage) {
    	
    	ModelImage [] kResult = new ModelImage[1];
    	kResult[0] = (ModelImage) blueImage.clone();
    	int i;
    	int nPyramid = 4;
    	int oneJacobiIter = 1;
    	int twoJacobiIter = 2;
    	float oneSmooth = 2e4f;
    	float twoSmooth = 2e5f;
    	boolean outputGainField = false;
    	int segType = 2; // Hard only
    	boolean cropBackground = false;
    	boolean regionFlag = true;
    	float centroid[];
    	float minimum, maximum; // For centroid estimation.
    	
    	// Estimate initial centroids
    	minimum = (float) blueImage.getMin();
    	maximum = (float) blueImage.getMax();
    	centroid = new float[nClasses];
    	for (i = 0; i < nClasses; i++) {
            centroid[i] = minimum + (maximum - minimum)*(i + 1)/(nClasses + 1);
    	}
    	
    	fireProgressStateChanged(5, null, "Segmenting blue channel image ...");
    	
    	AlgorithmFuzzyCMeans algoSeg = new AlgorithmFuzzyCMeans(kResult, blueImage, nClasses, nPyramid, oneJacobiIter,
    										twoJacobiIter, expValue, oneSmooth, twoSmooth, outputGainField, segType,
    										cropBackground, threshold, maxIter, endTol, regionFlag);
    	algoSeg.setCentroids(centroid);
    	
    	linkProgressToAlgorithm(algoSeg);
    	algoSeg.setProgressValues(generateProgressValues(6, 40));
        
    	algoSeg.setRunningInSeparateThread(false);
    	algoSeg.run();
 		algoSeg.finalize();
 		algoSeg = null;
 		
 		fireProgressStateChanged(40);
 		
 		fireProgressStateChanged(5, srcImage.getImageName(), "Subtracting 1 ...");
 		// Subtract 1 from image to make a binary image
 		AlgorithmImageMath algoSubtract = new AlgorithmImageMath(kResult[0], AlgorithmImageMath.SUBTRACT, 1.0, 0.0,
 												0.0, 1, true);
 		
 		linkProgressToAlgorithm(algoSubtract);
        algoSubtract.setProgressValues(generateProgressValues(41, 45));
 		algoSubtract.setRunningInSeparateThread(false);
 		algoSubtract.run();
 		algoSubtract.finalize();
 		algoSubtract = null;
 		
 		fireProgressStateChanged(45);
 		
 		return kResult[0];
    }
    
    private void particleAnalysis(ModelImage segImage) {
    	
    	fireProgressStateChanged(45, srcImage.getImageName(), "Particle analysis ...");
    	
    	AlgorithmMorphology2D algoMorph = new AlgorithmMorphology2D(segImage, kernelOpen, kernelSizeOpen,
    											kernelClose, kernelSizeClose, AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW,
    											itersOpen, itersClose, 0, 0, true, false);
    	
    	linkProgressToAlgorithm(algoMorph);
        algoMorph.setProgressValues(generateProgressValues(46, 95));
    	algoMorph.setRunningInSeparateThread(false);
    	algoMorph.run();
    	algoMorph.finalize();
    	algoMorph = null;
    	
    	fireProgressStateChanged(95);
    }
    
    private void findFociInNuclei(ModelImage redImage, ModelImage objImage) {
    	
    	float[] bufferA;
    	float[] bufferB;
    	float avgNoiseIntensity = 0;
    	int nBackgroundPixels = 0;
    	int[] extents = redImage.getExtents();
    	int length = extents[0] * extents[1];
    	int i, j;
    	int[] nRedPixelsInCell; // Number of red pixels in cell above threshold.
    	int[] nPixels; // Number of pixels in cell (blue channel).
    	float[] totalIntensityInCell; //Total intensity of red channel in cell above threshold
    	float[] avgIntensityInCell; // Average intensity inside each cell
    	float[] fnRatio; // F/N ratio for each cell
    	int nObjects; // Number of objects in an image
    	
    	bufferA = new float[length];
    	bufferB = new float[length];
    	try {
    		redImage.exportData(0, length, bufferA);
    	} catch (IOException Error) {
    		bufferA = null;
    		errorCleanUp("Algorithm Plugin Estimate Foci/Nuclei: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		bufferA = null;
    		errorCleanUp("Algorithm Plugin Estimate Foci/Nuclei: Out of Memory", true);
    		return;
    	}
    	
    	try {
    		objImage.exportData(0, length, bufferB);
    	} catch (IOException Error) {
    		bufferB = null;
    		errorCleanUp("Algorithm Plugin Estimate Foci/Nuclei: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		bufferB = null;
    		errorCleanUp("Algorithm Plugin Estimate Foci/Nuclei: Out of Memory", true);
    		return;
    	}
    	
    	// Estimate average background noise itensity in red channel image
    	for (i = 0; i < length; i++) {
    		if (bufferB[i] == 0) {
    			avgNoiseIntensity = avgNoiseIntensity + bufferA[i];
    			nBackgroundPixels++;
    		}
    	}
    	
    	avgNoiseIntensity = avgNoiseIntensity / nBackgroundPixels; // Threshold
    	bufferB = null;
    	
    	for (i = 0; i < length; i++) {
    		if (bufferA[i] <= avgNoiseIntensity) {
    			bufferA[i] = 0;
    		}
    	}
    	
    	// ID Objects
    	
    	idObjectsResultImage = (ModelImage) objImage.clone();
    	
    	AlgorithmMorphology2D algoIdObjects = new AlgorithmMorphology2D(idObjectsResultImage, 0, 0,
    												AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
    	algoIdObjects.setMinMax(minSize, maxSize);
    	algoIdObjects.setRunningInSeparateThread(false);
    	algoIdObjects.run();
    	algoIdObjects.finalize();
    	algoIdObjects = null;
    	
    	// Scan red image (noise removed) and id objects image to find red pixels in cell
    	try {
    		bufferB = new float[length];
    		idObjectsResultImage.exportData(0, length, bufferB);
    	} catch (IOException Error) {
    		bufferB = null;
    		errorCleanUp("Algorithm Plugin Estimate Foci/Nuclei: Image(s) Locked", true);
    		return;
    	} catch (OutOfMemoryError e) {
    		bufferB = null;
    		errorCleanUp("Algorithm Plugin Estimate Foci/Nuclei: Out of Memory", true);
    		return;
    	}
    	
    	nObjects = (int) idObjectsResultImage.getMax();
    	nRedPixelsInCell = new int[nObjects];
    	nPixels = new int[nObjects];
    	totalIntensityInCell = new float[nObjects];
    	avgIntensityInCell = new float[nObjects];
    	fnRatio = new float[nObjects];
    	
    	for (j = 1; j <= nObjects; j++) {
    		for (i = 0; i < length; i++) {
    			if (bufferB[i] == j) {
    				nPixels[j - 1] = nPixels[j - 1] + 1;
    				if (bufferA[i] != 0) {
    					nRedPixelsInCell[j - 1] = nRedPixelsInCell[j - 1] + 1;
        				totalIntensityInCell[j - 1] = totalIntensityInCell[j - 1] + bufferA[i];
    				}	
    			}
    		}
    		avgIntensityInCell[j - 1] = totalIntensityInCell[j - 1] / nRedPixelsInCell[j - 1];
    		fnRatio[j - 1] = (float) nRedPixelsInCell[j - 1] / nPixels[j - 1];
    	}
    	
    	// Printing the results in Output window
    	String mStr;
        float area;

        mStr = srcImage.getFileInfo(0).getAreaUnitsOfMeasureStr();
    	
        ViewUserInterface.getReference().getMessageFrame().clear(0);
    	ViewUserInterface.getReference().getMessageFrame().getData().append("\nIdentified " + nObjects + 
    																			" objects. \n");
    	ViewUserInterface.getReference().getMessageFrame().getData().append(" Object \t# of pixels (Blue)\tArea(" + mStr +
        							")\t# of pixels (Red)\tTotal red intensity\t Average red intensity\t F/N \n");
    	
    	for (i = 1; i <= nObjects; i++) {
    		area = nPixels[i-1] * srcImage.getFileInfo(0).getResolution(0) * srcImage.getFileInfo(0).getResolution(1);
    		ViewUserInterface.getReference().getMessageFrame().getData().append("    " + i + "\t" + nPixels[i-1] + "\t\t" +
    							area + "\t\t" + nRedPixelsInCell[i-1] + "\t\t" + totalIntensityInCell[i-1] + "\t\t" +
    							avgIntensityInCell[i-1] + "\t\t" + fnRatio[i-1] + "\n");
    	}
    	
    	fireProgressStateChanged(100);
    	
    }
    
    /**
     * This method returns the result image
     */
    public ModelImage getResultImage() {
    	
    	resultImage = (ModelImage) idObjectsResultImage.clone();
    	return resultImage;
    }
    
}
