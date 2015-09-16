import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.filters.AlgorithmAnisotropicDiffusion;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR25D2;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmCrop;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JTextArea;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmCellFiring extends AlgorithmBase {
    
    
    private int xDim;
    
    private int yDim;
    
    private int zDim;
    
    private boolean alreadyDisplayed;
    
    private boolean displayInputImage;
    
    private float downSampleXY;
    
    private float downSampleZ;
    
    private boolean displayDownSampleImage;
    
    private boolean saveDownSampleImage;
    
    private boolean cropImage;
    
    private boolean registerImage;
    
    private int earliestSlices;
    
    private boolean anisotropicDiffusion;

    private final JTextArea outputTextArea;
    

    public PlugInAlgorithmCellFiring(ModelImage image, boolean alreadyDisplayed, boolean displayInputImage, 
    		float downSampleXY, float downSampleZ, boolean displayDownSampleImage, 
    		boolean saveDownSampleImage,  boolean cropImage, boolean registerImage, 
    		int earliestSlices, boolean anisotropicDiffusion, final JTextArea outputTextArea) {
    	super(null, image);
    	this.alreadyDisplayed = alreadyDisplayed;
    	this.displayInputImage = displayInputImage;
    	this.downSampleXY = downSampleXY;
    	this.downSampleZ = downSampleZ;
    	this.displayDownSampleImage = displayDownSampleImage;
    	this.saveDownSampleImage = saveDownSampleImage;
    	this.cropImage = cropImage;
    	this.registerImage = registerImage;
    	this.earliestSlices = earliestSlices;
        this.anisotropicDiffusion = anisotropicDiffusion;
        this.outputTextArea = outputTextArea;
    }

    @Override
    public void runAlgorithm() {
    	
        outputTextArea.append("Running Algorithm v1.0" + "\n");

        final long begTime = System.currentTimeMillis();
        
        if (displayInputImage && (!alreadyDisplayed)) {
        	new ViewJFrameImage(srcImage);
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        ModelImage presentImage = srcImage;
        
        if ((downSampleXY < 1.0f) || (downSampleZ < 1.0f)) {
        	int interp = AlgorithmTransform.TRILINEAR;
        	TransMatrix xfrm = new TransMatrix(4);
        	xfrm.identity();
            int newExtents[] = new int[3];
            newExtents[0] = Math.round(downSampleXY * xDim);
            newExtents[1] = Math.round(downSampleXY * yDim);
            newExtents[2] = Math.round(downSampleZ * zDim);
            float oXres = srcImage.getFileInfo(0).getResolutions()[0] * xDim / newExtents[0];
            float oYres = srcImage.getFileInfo(0).getResolutions()[1] * yDim / newExtents[1];
            float oZres = srcImage.getFileInfo(0).getResolutions()[2] * zDim / newExtents[2];
            int units[] = srcImage.getUnitsOfMeasure();
            final boolean doClip = true;
            final boolean doPad = false;
            final boolean doVOI = cropImage;
            if (cropImage) {
            	// Propagate a single 2D slice VOI to all slices so it will not be removed by down sampling 
                VOIVector VOIs = presentImage.getVOIs();
	            if ((VOIs != null) && (VOIs.size() > 0)) {
	            	VOI rectVOI = VOIs.get(0);
	            	if (rectVOI != null) {
	            		int xBounds[] = new int[2];
	            		int yBounds[] = new int[2];
	            		int zBounds[] = new int[2];
	            	    rectVOI.getBounds(xBounds, yBounds, zBounds);
	            	    VOIBaseVector curves = rectVOI.getCurves();
	            	    VOIBase vBase = curves.get(0);
	            	    int nPoints = vBase.size();
	            	    float xArr[] = new float[nPoints];
	            	    float yArr[] = new float[nPoints];
	            	    float zArr[] = new float[nPoints];
	            	    for (int i = 0; i < nPoints; i++) {
	            	    	xArr[i] = vBase.get(i).X;
	            	    	yArr[i] = vBase.get(i).Y;
	            	    }
	            	    for (int z = 0; z < zBounds[0]; z++) {
	            	        for (int i = 0; i < nPoints; i++) {
	            	            zArr[i] = z;	
	            	        }
	            	        rectVOI.importCurve(xArr, yArr, zArr);
	            	    }
	            	    for (int z = zBounds[1]+1; z < presentImage.getExtents()[2]; z++) {
	            	        for (int i = 0; i < nPoints; i++) {
	            	            zArr[i] = z;	
	            	        }
	            	        rectVOI.importCurve(xArr, yArr, zArr);
	            	    }
	            	}
	            }
            } // if (cropImage)
            final boolean doRotateCenter = false;
            final Vector3f center = new Vector3f();
            final float fillValue = 0.0f;
            final boolean doUpdateOrigin = false;
            final boolean isSATransform = false;
            AlgorithmTransform algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oZres,
            		newExtents[0], newExtents[1], newExtents[2], units,
                    doVOI, doClip, doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.setUseScannerAnatomical(isSATransform);
            algoTrans.setSuppressProgressBar(true);

            algoTrans.run();
            
            ModelImage downSampleImage = algoTrans.getTransformedImage();
            downSampleImage.calcMinMax();
            presentImage = downSampleImage;
            algoTrans.disposeLocal();
            algoTrans = null;
            
            if (displayDownSampleImage) {
            	new ViewJFrameImage(downSampleImage);
            }
            
            if (saveDownSampleImage) {
            	final FileIO io = new FileIO();
	            io.setQuiet(true);
	            io.setSuppressProgressBar(true);
	            final FileWriteOptions options = new FileWriteOptions(null, null, true);
	            options.setFileType(FileUtility.TIFF);
	
	            options.setIsScript(true);
	            options.setOptionsSet(true);
	            
	            options.setFileDirectory(srcImage.getImageDirectory() + File.separator);
	            int index = srcImage.getImageFileName().indexOf(".");
	            String baseName;
	            if (index > 0) {
	                baseName = srcImage.getImageFileName().substring(0, index);	
	            }
	            else {
	            	baseName = srcImage.getImageFileName();
	            }
	            options.setBeginSlice(0);
                options.setEndSlice(downSampleImage.getExtents()[2] - 1);
                options.setFileName(baseName + "_XY" + String.valueOf(downSampleXY) + "_Z" +
	                                 String.valueOf(downSampleZ) + ".tif");
                boolean allowScriptRecording = false;
                io.writeImage(downSampleImage, options, false, allowScriptRecording);
            } // if (saveDownSampleImage)
        } // if ((downSampleXY < 1.0f) || (downSampleZ < 1.0f))
        
        if (cropImage) {
        	VOIVector VOIs = presentImage.getVOIs();
            if (VOIs != null) {
            	VOI rectVOI = VOIs.get(0);
            	if (rectVOI != null) {
            		int xBounds[] = new int[2];
            		int yBounds[] = new int[2];
            		int zBounds[] = new int[2];
            	    rectVOI.getBounds(xBounds, yBounds, zBounds);
            	    // Do not use zBounds
            	    zBounds[0] = 0;
            	    zBounds[1] = presentImage.getExtents()[2] - 1;
            	    int cropExtents[] = new int[3];
            	    cropExtents[0] = xBounds[1] - xBounds[0] + 1;
            	    cropExtents[1] = yBounds[1] - yBounds[0] + 1;
            	    cropExtents[2] = presentImage.getExtents()[2];
            	    ModelImage croppedImage = new ModelImage(srcImage.getDataType(), cropExtents, srcImage.getImageName() + "_crop");
            	    // Extra space around VOI bounds in x and y dimensions
            	    int cushion = 0;
            	    AlgorithmCrop cropAlgo = new AlgorithmCrop(croppedImage, presentImage, cushion, xBounds, yBounds, zBounds);
            	    
            	    cropAlgo.run();
            	    try {
    					new ViewJFrameImage(croppedImage, null, new Dimension(610,
    							200));
    				} catch (OutOfMemoryError error) {
    					MipavUtil
    							.displayError("Out of memory: unable to open new crop image frame");
    					setCompleted(false);
    					return;
    				}
            	    presentImage = croppedImage;
            	    cropAlgo.finalize();
            	    cropAlgo = null;
            	}
            	else {
            		MipavUtil.displayError("No VOI is present");
            		setCompleted(false);
            		return;
            	}
            } // if (VOIs != null)
            else {
            	MipavUtil.displayError("No VOI vector is present");
            	setCompleted(false);
            	return;
            }
        } // if (cropImage)
        
        if (registerImage) {
        	// Use first slice as reference
        	boolean doAdjacent = false;
        	int refImageNum = 0;
        	int  cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
        	int DOF = 3;
        	int interp = AlgorithmTransform.BILINEAR;
        	int interp2 = AlgorithmTransform.BILINEAR;
        	float rotateBegin = -3.0f;
        	float rotateEnd = 3.0f;
        	float coarseRate = 3.0f;
        	float fineRate = 2.0f;
        	boolean doGraph = false;
        	boolean doSubsample = true;
        	boolean transformVOIs = false;
        	int maxIterations = 2;
        	int numMinima = 6;
        	AlgorithmRegOAR25D2 reg25 = new AlgorithmRegOAR25D2(presentImage, cost, DOF, interp, interp2, doAdjacent, refImageNum,
                    rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample,
                    transformVOIs, maxIterations, numMinima);
        	reg25.run();
        	if (reg25 != null) {
                reg25.disposeLocal();
                reg25.finalize();
            }
        	reg25 = null;
        } // if (registerImage)
        
        int presentXDim = presentImage.getExtents()[0];
        int presentYDim = presentImage.getExtents()[1];
        int presentArea = presentXDim * presentYDim;
        int presentZDim = presentImage.getExtents()[2];
        int presentVolume = presentArea * presentZDim;
        float buffer[];
        try {
            buffer = new float[presentVolume];
        }
        catch (OutOfMemoryError e) {
        	MipavUtil.displayError("Out of memory: unable to create float buffer = new float[presentVolume]");
	        setCompleted(false);
	        return;	
        }
        try {
        	presentImage.exportData(0, presentVolume, buffer);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on presentImage.exportData(0, presentVolume, buffer");
        	setCompleted(false);
        	return;
        }
        
        
        float areaBuffer[];
        try {
        	areaBuffer = new float[presentArea];
        }
        catch (OutOfMemoryError e) {
        	MipavUtil.displayError("Out of memory: unable to create float areaBuffer = new float[presentArea]");
	        setCompleted(false);
	        return;	
        }
        
        for (int z = 0; z < earliestSlices; z++) {
            for (int i = 0; i < presentArea; i++) {
            	areaBuffer[i] += buffer[i + z * presentArea];
            }
        }
        
        for (int i = 0; i < presentArea; i++) {
        	areaBuffer[i] = areaBuffer[i]/earliestSlices;
        }
        
        for (int z = 0; z < presentZDim; z++) {
        	for (int i = 0; i < presentArea; i++) {
        	    buffer[i + z * presentArea] -= areaBuffer[i];	
        	}
        }
        
        try {
        	presentImage.importData(0, buffer, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on presentImage.import(0, buffer, true");
        	setCompleted(false);
        	return;
        }
        
        if (anisotropicDiffusion) {
            float sigmaX = 1.0f;
            float sigmaY = 1.0f;
            float sigmaZ = 1.0f;
            boolean useCorrectionFactor = true;
            if (useCorrectionFactor) {
                sigmaZ = sigmaZ * (presentImage.getFileInfo()[0].getResolution(0)/presentImage.getFileInfo()[0].getResolution(2));	
            } // if (useCorrectionFactor)
            float sigmaArray[] = new float[3];
            sigmaArray[0] = sigmaX;
            sigmaArray[1] = sigmaY;
            sigmaArray[2] = sigmaZ;
            int iterations = 10;
            float kValue = 15.0f;
            boolean entireImage = true;
            boolean image25D = false;
            AlgorithmAnisotropicDiffusion anisotropicAlgo = new AlgorithmAnisotropicDiffusion(presentImage, sigmaArray, iterations,
            		                kValue, entireImage, image25D);
            anisotropicAlgo.run();
            anisotropicAlgo.finalize();
            anisotropicAlgo = null;
        } // if (anisotropicDiffusion)
        

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

    }
    


}
